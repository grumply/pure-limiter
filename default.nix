{ mkDerivation, base, containers, pure-json, pure-time, stdenv
}:
mkDerivation {
pname = "pure-limiter";
version = "0.7.0.0";
src = ./.;
libraryHaskellDepends = [
base containers pure-json pure-time
];
homepage = "github.com/grumply/pure-limiter";
license = stdenv.lib.licenses.bsd3;
}
