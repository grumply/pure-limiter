{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
module Pure.Data.Limiter (Limiter,Limiter_(..),Tokens,limiter,limit,minDelay) where

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

type Limiter = MVar Limiter_
data Limiter_ = Limiter
  { bValue        :: {-# UNPACK #-}!Tokens
  , bMax          :: {-# UNPACK #-}!Tokens
  , bLastUpdate   :: {-# UNPACK #-}!Micros
  , bRefillTime   :: {-# UNPACK #-}!Micros
  , bRefillAmount :: {-# UNPACK #-}!Tokens
  } deriving (Eq,Generic,ToJSON,FromJSON)

limiter :: Int -> Int -> Int -> Micros -> IO Limiter
limiter bMax bValue bRefillAmount bRefillTime = do
  bLastUpdate <- micros
  newMVar Limiter {..}

{-# INLINE reduce #-}
reduce :: Int -> Limiter -> IO Bool
reduce tokens bucket = modifyMVar bucket $ \Limiter {..} -> do
  now  <- micros
  let last = round $ getMicros bLastUpdate
      val  = bValue
      refillTime :: Int
      refillTime = round $ getMicros bRefillTime
      refillCount = (round (getMicros now) - last) `div` refillTime
      value = min bMax (abs {- protect from overflow -} $ val + refillCount * bRefillAmount)
      lu    = min now (Micros $ fromIntegral $ last + refillCount * refillTime)
      limited = tokens > value
  return (Limiter { bValue = if not limited then value - tokens else value, bLastUpdate = lu, .. },not limited)

{-# INLINE limit #-}
limit :: Int -> Limiter -> IO a -> IO (Maybe a)
limit tokens bucket f = do
  b <- Pure.Data.Limiter.reduce tokens bucket
  if b
    then Just <$> f
    else return Nothing

{-# INLINE minDelay #-}
minDelay :: Int -> Limiter -> IO Micros
minDelay tokens bucket = do
  Limiter {..} <- readMVar bucket
  if bValue > tokens
    then return 0
    else do
      now <- micros
      let lu = bLastUpdate
          needed = tokens - bValue
          rounds = needed `div` bRefillAmount
      return (bRefillTime * fromIntegral rounds - (now - lu))
