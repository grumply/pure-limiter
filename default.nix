{ mkDerivation, base, containers, pure-json, pure-time, transformers, stdenv
}:
mkDerivation {
pname = "pure-limiter";
version = "0.8.0.0";
src = ./.;
libraryHaskellDepends = [
base containers pure-json pure-time transformers
];
homepage = "github.com/grumply/pure-limiter";
license = stdenv.lib.licenses.bsd3;
}
