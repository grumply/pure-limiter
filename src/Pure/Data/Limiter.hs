{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
module Pure.Data.Limiter (Bucket,Bucket_(..),Tokens,limiter,limit,minDelay) where

-- from pure-time
import Pure.Data.Time

-- from pure-json
import Pure.Data.JSON

-- from base
import Control.Concurrent
import GHC.Generics

-- Inspired by: https://medium.com/smyte/rate-limiter-df3408325846

-- In multi-threaded contexts, be sure that all rate-limited actions use the same number of tokens.
-- Or be sure that you understand when this could be a problem (starvation). Also understand
-- that relying heavily on minDelay can lead to high contention - if needed, consider adding jitter
-- with the normaldistribution package on hackage

type Tokens = Int

type Bucket = MVar Bucket_
data Bucket_ = Bucket
  { bValue        :: {-# UNPACK #-}!Tokens
  , bLastUpdate   :: {-# UNPACK #-}!Micros
  , bBurst        :: {-# UNPACK #-}!Tokens
  , bRefillTime   :: {-# UNPACK #-}!Micros
  , bRefillAmount :: {-# UNPACK #-}!Tokens
  } deriving (Eq,Generic,ToJSON,FromJSON)

limiter :: Int -> Int -> Micros -> IO Bucket
limiter bBurst bRefillAmount bRefillTime = do
  bLastUpdate <- micros
  let bValue = bBurst
  newMVar Bucket {..}

{-# INLINE reduce #-}
reduce :: Int -> Bucket -> IO Bool
reduce tokens bucket = modifyMVar bucket $ \Bucket {..} -> do
  now  <- micros
  let last = round $ getMicros bLastUpdate
      val  = bValue
      refillTime :: Int
      refillTime = round $ getMicros bRefillTime
      refillCount = (round (getMicros now) - last) `div` refillTime
      value = min bBurst (abs {- protect from overflow -} $ val + refillCount * bRefillAmount)
      lu    = min now (Micros $ fromIntegral $ last + refillCount * refillTime)
  return (Bucket { bValue = value, bLastUpdate = lu, .. },tokens <= value)

{-# INLINE limit #-}
limit :: Int -> Bucket -> IO a -> IO (Maybe a)
limit tokens bucket f = do
  b <- Pure.Data.Limiter.reduce tokens bucket
  if b
    then Just <$> f
    else return Nothing

{-# INLINE minDelay #-}
minDelay :: Int -> Bucket -> IO Micros
minDelay tokens bucket = do
  Bucket {..} <- readMVar bucket
  if bValue > tokens
    then return 0
    else do
      now <- micros
      let lu = bLastUpdate
          needed = tokens - bValue
          rounds = needed `div` bRefillAmount
      return (bRefillTime * fromIntegral rounds - (now - lu))
