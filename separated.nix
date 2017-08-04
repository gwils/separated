{ mkDerivation, base, bifunctors, directory, doctest, filepath
, lens, parsec, QuickCheck, semigroupoids, semigroups, stdenv
, template-haskell
}:
mkDerivation {
  pname = "separated";
  version = "0.2.1";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors lens semigroupoids semigroups
  ];
  testHaskellDepends = [
    base directory doctest filepath parsec QuickCheck template-haskell
  ];
  homepage = "https://github.com/data61/separated";
  description = "A data type with elements separated by values";
  license = stdenv.lib.licenses.bsd3;
}
