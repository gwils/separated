{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module Data.Separated(
-- * Data types
  Separated(..)
, Separated1(..)
, Pesarated(..)
, Pesarated1(..)
-- * Iso
, separated
, separated1
, pesarated
, pesarated1
-- * Viewing
, HasHead(..)
, HasTail(..)
-- * Constructing
, Separated1Single(..)
, Pesarated1Single(..)
, Construct(..)
, SeparatedCons(..)
, PesaratedCons(..)
-- * Appending
, Appends(..)
-- * Alternating
, separatedBy
, separatedBy1
, pesaratedBy
, pesaratedBy1
) where

import Control.Applicative(Applicative((<*>), pure), Alternative(many))
import Control.Category(Category((.), id))
import Control.Lens(Swapped(swapped), Iso, Lens, iso, from, (#), (^.), _1, _2, makeWrapped, _Wrapped)
import Data.Bifoldable(Bifoldable(bifoldr))
import Data.Bifunctor(Bifunctor(bimap), first)
import Data.Bitraversable(Bitraversable(bitraverse))
import Data.Eq(Eq)
import Data.Foldable(Foldable, foldr)
import Data.Functor(Functor(fmap), (<$>))
import Data.Functor.Apply as Apply(Apply((<.>)))
import Data.List(intercalate, zipWith, repeat)
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord)
import Data.Semigroup as Semigroup(Semigroup((<>)))
import Data.String(String)
import Data.Traversable(Traversable, traverse)
import Data.Tuple(fst, snd, uncurry)
import Prelude(Show(show), const, flip)

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XFlexibleContexts
-- >>> import Control.Lens(set)
-- >>> import Control.Monad(return)
-- >>> import Data.Char(toUpper)
-- >>> import Data.Either(isLeft)
-- >>> import Prelude hiding (id, (.))
-- >>> import Text.Parsec(parse, char, digit)
-- >>> import Test.QuickCheck(Arbitrary(..))
-- >>> instance (Arbitrary s, Arbitrary a) => Arbitrary (Separated s a) where arbitrary = fmap (^. separated) arbitrary
-- >>> instance (Arbitrary a, Arbitrary s) => Arbitrary (Separated1 s a) where arbitrary = do a <- arbitrary; x <- arbitrary; return ((a, x) ^. separated1)
-- >>> instance (Arbitrary s, Arbitrary a) => Arbitrary (Pesarated s a) where arbitrary = fmap (^. pesarated) arbitrary
-- >>> instance (Arbitrary s, Arbitrary a) => Arbitrary (Pesarated1 s a) where arbitrary = fmap (^. pesarated1) arbitrary
-- >>> let emptySeparated :: Separated s a; emptySeparated = empty
-- >>> let emptyPesarated :: Pesarated s a; emptyPesarated = empty

-- | A list of pairs of separator and value. Separated by @a@ in values @b@.
-- There are an even number of separators as there are values.
newtype Separated a b =
  Separated [(a, b)]
  deriving (Eq, Ord)

-- | A list of pairs of separator and value. Separated by @a@ in values @b@.
-- There is one more value than there are separators.
data Separated1 b a =
  Separated1 b (Separated a b)
  deriving (Eq, Ord)

-- | The @Separated@ type constructor, flipped.
newtype Pesarated b a =
  Pesarated (Separated a b)
  deriving (Eq, Ord)

-- | The @Separated1@ type constructor, flipped.
newtype Pesarated1 a b =
  Pesarated1 (Separated1 b a)
  deriving (Eq, Ord)

-- | The isomorphism to a list of pairs of element and separator values.
--
-- >>> separated # emptySeparated
-- []
--
-- >>> separated # ('x' +: 6 +: emptySeparated)
-- [('x',6)]
--
-- >>> [] ^. separated
-- []
--
-- >>> [(6, [])] ^. separated
-- [6,[]]
separated ::
  Iso [(a, b)] [(c, d)] (Separated a b) (Separated c d)
separated =
  from _Wrapped

-- | The isomorphism to element values interspersed with a separator.
--
-- >>> separated1 # (singleSeparated 6)
-- (6,[])
--
-- >>> separated1 # (5 +: 'x' +: singleSeparated 6)
-- (5,['x',6])
--
-- >>> (6, emptySeparated) ^. separated1
-- [6]
--
-- >>> (5, 'x' +- 6) ^. separated1
-- [5,'x',6]
separated1 ::
  Iso (a, Separated s a) (b, Separated t b) (Separated1 a s) (Separated1 b t)
separated1 =
  iso (uncurry Separated1) (\(Separated1 a x) -> (a, x))

-- | The isomorphism to element values interspersed with a separator.
--
-- >>> pesarated # emptyPesarated
-- []
--
-- >>> ('a', 'x' +- 6) ^. pesarated1
-- ['a',6,'x']
--
-- >>> ('x' -: 6 -: emptyPesarated)
-- ['x',6]
pesarated ::
  Iso [(a, b)] [(c, d)] (Pesarated b a) (Pesarated d c)
pesarated =
  separated . from _Wrapped

-- | The isomorphism to element values interspersed with a separator.
--
-- >>> pesarated1 # singlePesarated 6
-- (6,[])
--
-- >>> pesarated1 # (8 -: 'x' -: singlePesarated 6)
-- (8,['x',6])
--
-- >>> (6, empty) ^. pesarated1
-- [6]
--
-- >>> (5, 'x' -+ 6) ^. pesarated1
-- [5,'x',6]
pesarated1 ::
  Iso (a, Pesarated a s) (b, Pesarated b t) (Pesarated1 s a) (Pesarated1 t b)
pesarated1 =
  iso
    (\(a, Pesarated x) -> Pesarated1 (Separated1 a x))
    (\(Pesarated1 (Separated1 a x)) -> (a, Pesarated x))

-- | Structures that have a head element.
class HasHead s t a b | s -> a, t -> b, s b -> t, t a -> s where
  headL ::
    Lens s t a b

-- | A lens on the first element value.
--
-- >>> (singleSeparated 7 :: Separated1 Int Char) ^. headL
-- 7
--
-- prop> (singleSeparated x :: Separated1 Int Char) ^. headL == (x :: Int)
instance HasHead (Separated1 a t) (Separated1 a t) a a where
  headL =
    from separated1 . _1

-- | A lens on the first element value.
--
-- >>> (singlePesarated 7 :: Pesarated1 Char Int) ^. headL
-- 7
--
-- prop> (singlePesarated x :: Pesarated1 Char Int) ^. headL == (x :: Int)
instance HasHead (Pesarated1 a t) (Pesarated1 a t) t t where
  headL =
    _Wrapped . headL

-- | Structures that have a tail.
class HasTail s t a b | s -> a, t -> b, s b -> t, t a -> s where
  tailL ::
    Lens s t a b

-- | A lens on the tail.
--
-- prop> (d +: e +: (singleSeparated x :: Separated1 Int Char)) ^. tailL == e +: x +: emptySeparated
instance HasTail (Separated1 a s) (Separated1 a t) (Separated s a) (Separated t a) where
  tailL =
    from separated1 . _2

-- | A lens on the tail.
--
-- prop> (d -: e -: (singlePesarated x :: Pesarated1 Char Int)) ^. tailL == e -: x -: emptyPesarated
instance HasTail (Pesarated1 a s) (Pesarated1 b s) (Pesarated s a) (Pesarated s b) where
  tailL =
    _Wrapped . tailL . from _Wrapped

-- | Construct a single separated value.
class Separated1Single f where
  singleSeparated ::
    a
    -> f a s

-- | Zero element values interspersed with one element.
--
-- >>> (singleSeparated 4 :: Separated1 Int Char)
-- [4]
--
-- prop> (singleSeparated x  :: Separated1 Int Char) ^. tailL == emptySeparated
instance Separated1Single Separated1 where
  singleSeparated a =
    Separated1 a mempty

class Pesarated1Single f where
  singlePesarated ::
    a
    -> f s a

-- | Zero element values interspersed with one element.
--
-- >>>  (singlePesarated 4 :: Pesarated1 Char Int)
-- [4]
--
-- prop> (singlePesarated x  :: Pesarated1 Int Char) ^. tailL == emptyPesarated
instance Pesarated1Single Pesarated1 where
  singlePesarated a =
    Pesarated1 (Separated1 a mempty)

-- | Construction of separated values.
class Construct f where
  (+-) ::
    s
    -> a
    -> f s a
  (-+) ::
    s
    -> a
    -> f a s
  empty ::
    f s a
  sprinkle ::
    s
    -> [a]
    -> f s a

infixl 9 +-

-- | One element and one separator.
--
-- >>> (7 +- "abc") :: Separated Int String
-- [7,"abc"]
--
-- >>> 7 +: "abc" +: (8 +- "def") :: Separated Int String 
-- [7,"abc",8,"def"]
instance Construct Separated where
  s +- a =
    Separated [(s, a)]
  s -+ a =
    swapped # (s +- a)
  empty =
    Separated mempty
  sprinkle s as =
    Separated
      ((,) s <$> as)

-- | One element and one separator.
--
-- >>> (7 -+ "abc") :: Pesarated String Int
-- [7,"abc"]
--
-- >>> 7 -: "abc" -: (8 -+ "def") :: Pesarated String Int
-- [7,"abc",8,"def"]
instance Construct Pesarated where
  s +- a =
    swapped . _Wrapped # (s +- a)
  s -+ a =
    _Wrapped # (s +- a)
  empty =
    Pesarated empty
  sprinkle s as =
    Pesarated
      (swapped # sprinkle s as)

-- | Prepend a value to a separated-like structure.
class (f ~ SeparatedConsF g, g ~ SeparatedConsG f) => SeparatedCons f g where
  type SeparatedConsF g :: * -> * -> *
  type SeparatedConsG f :: * -> * -> *
  (+:) ::
    a
    -> f b a
    -> g a b
  
infixr 5 +:

instance SeparatedCons Separated1 Separated where
  type SeparatedConsF Separated = Separated1
  type SeparatedConsG Separated1 = Separated
  s +: Separated1 a (Separated x) =
    Separated ((s, a) : x)

instance SeparatedCons Separated Separated1 where
  type SeparatedConsF Separated1 = Separated
  type SeparatedConsG Separated = Separated1
  (+:) =
    Separated1

-- | Prepend a value to a separated-like structure.
class (f ~ PesaratedConsF g, g ~ PesaratedConsG f) => PesaratedCons f g where
  type PesaratedConsF g :: * -> * -> *
  type PesaratedConsG f :: * -> * -> *
  (-:) ::
    b
    -> f b a
    -> g a b
  
infixr 5 -:

instance PesaratedCons Pesarated Pesarated1 where
  type PesaratedConsF Pesarated1 = Pesarated
  type PesaratedConsG Pesarated = Pesarated1
  b -: (Pesarated x) =
    Pesarated1
      (b +: x)

instance PesaratedCons Pesarated1 Pesarated where
  type PesaratedConsF Pesarated = Pesarated1
  type PesaratedConsG Pesarated1 = Pesarated
  b -: (Pesarated1 x) =
    Pesarated (b +: x)

-- | Append two to make one.
class Appends a b c | a b -> c where
  (<++>) ::
    a
    -> b
    -> c

infixr 5 <++>

-- | Append two lists of separated values to produce a list of pairs of separator and element values.
--
-- >>> (singleSeparated 7 :: Separated1 Int Char) <++> (singleSeparated 'a' :: Separated1 Char Int)
-- [7,'a']
--
-- 'a' +: (singleSeparated 7 :: Separated1 Int Char) <++> (singleSeparated 'b' :: Separated1 Char Int)
-- ['a',7,'b']
--
-- prop> a +: (b :: Separated Int Int) == a +: b --  (a +: (b <++> c)) == ((a +: b) <++> c)
instance Appends (Separated1 s a) (Separated1 a s) (Separated s a) where
  Separated1 s x <++> Separated1 t (Separated y) =
    let (q, r') = (s, x) ^. separated1 . shift
    in Separated (q <> ((r', t) : y))

-- | Append element values interspersed with a separator to a list of pairs of separator and element values.
--
-- >>> (emptySeparated :: Separated Int Char) <++> (singleSeparated 7 :: Separated1 Int Char)
-- [7]
--
-- >>> (emptySeparated :: Separated Int Char) <++> 6 +: 'x' +: (singleSeparated 7 :: Separated1 Int Char)
-- [6,'x',7]
--
-- >>> 'w' +: (emptySeparated :: Separated Int Char) <++> 6 +: 'x' +: (singleSeparated 7 :: Separated1 Int Char)
-- ['w',6,'x',7]
instance Appends (Separated s a) (Separated1 s a) (Separated1 s a) where
  Separated x <++> Separated1 t y =
    let (z, w') = separated1 . shift # (x, t)
    in Separated1 z (w' <> y)

-- | Append a list of pairs of separator and element values to element values interspersed with a separator.
--
-- >>> (singleSeparated 7 :: Separated1 Int Char) <++> (emptySeparated :: Separated Char Int)
-- [7]
--
-- >>> (singleSeparated 6 :: Separated1 Int Char) <++> 'x' +: 7 +: (emptySeparated :: Separated Char Int)
-- [6,'x',7]
--
-- >>> 'w' +: (singleSeparated 6 :: Separated1 Int Char) <++> 'x' +: 7 +: (emptySeparated :: Separated Char Int)
-- ['w',6,'x',7]
instance Appends (Separated1 a s) (Separated s a) (Separated1 a s) where
  Separated1 a x <++> y =
    Separated1 a (x <> y)

-- | Alternate separated values e.g. `f ~ Parser`.
--
-- >>> parse (separatedBy (char ',') digit) "test" ""
-- Right []
--
-- >>> isLeft (parse (separatedBy (char ',') digit) "test" ",")
-- True
--
-- >>> parse (separatedBy (char ',') digit) "test" ",1"
-- Right [',','1']
--
-- >>> isLeft (parse (separatedBy (char ',') digit) "test" ",1,")
-- True
--
-- >>> parse (separatedBy (char ',') digit) "test" ",1,2,3,4,5"
-- Right [',','1',',','2',',','3',',','4',',','5']
separatedBy ::
  Alternative f =>
  f a
  -> f b
  -> f (Separated a b)
separatedBy a b =
  Separated <$>
    many
      ((,) <$> a <*> b)

-- | Alternate separated values e.g. `f ~ Parser`.
--
-- >>> isLeft (parse (separatedBy1 (char ',') digit) "test" "")
-- True
--
-- >>> parse (separatedBy1 (char ',') digit) "test" ","
-- Right [',']
--
-- >>> isLeft (parse (separatedBy1 (char ',') digit) "test" ",1")
-- True
--
-- >>> parse (separatedBy1 (char ',') digit) "test" ",1,"
-- Right [',','1',',']
--
-- >>>  parse (separatedBy1 (char ',') digit) "test" ",1,2,3,4,5,"
-- Right [',','1',',','2',',','3',',','4',',','5',',']
separatedBy1 ::
  Alternative f =>
  f b
  -> f a
  -> f (Separated1 b a)
separatedBy1 b a =
  Separated1 <$> b <*> separatedBy a b

-- | Alternate separated values e.g. `f ~ Parser`.
--
-- >>> parse (pesaratedBy (char ',') digit) "test" ""
-- Right []
--
-- >>> isLeft (parse (pesaratedBy (char ',') digit) "test" ",")
-- True
--
-- >>> parse (separatedBy (char ',') digit) "test" ",1"
-- Right [',','1']
--
-- >>> isLeft (parse (pesaratedBy (char ',') digit) "test" ",1,")
-- True
--
-- >>> parse (pesaratedBy (char ',') digit) "test" ",1,2,3,4,5"
-- Right [',','1',',','2',',','3',',','4',',','5']
pesaratedBy ::
  Alternative f =>
  f a
  -> f b
  -> f (Pesarated b a)
pesaratedBy a b =
  Pesarated <$> separatedBy a b

-- | Alternate separated values e.g. `f ~ Parser`.
--
-- >>> isLeft (parse (pesaratedBy1 (char ',') digit) "test" "")
-- True
--
-- >>> parse (pesaratedBy1 (char ',') digit) "test" ","
-- Right [',']
--
-- >>> isLeft (parse (pesaratedBy1 (char ',') digit) "test" ",1")
-- True
--
-- >>> parse (pesaratedBy1 (char ',') digit) "test" ",1,"
-- Right [',','1',',']
--
-- >>>  parse (pesaratedBy1 (char ',') digit) "test" ",1,2,3,4,5,"
-- Right [',','1',',','2',',','3',',','4',',','5',',']
pesaratedBy1 ::
  Alternative f =>
  f b
  -> f a
  -> f (Pesarated1 a b)
pesaratedBy1 b a =
  Pesarated1 <$> separatedBy1 b a

-- | The isomorphism that swaps elements with their separators.
--
-- >>> swapped # emptySeparated
-- []
--
-- >>> swapped # ('x' +: 6 +: emptySeparated)
-- [6,'x']
--
-- >>> emptySeparated ^. swapped
-- []
--
-- >>> ('x' +: 6 +: emptySeparated) ^. swapped
-- [6,'x']
instance Swapped Separated where
  swapped =
    let swap = (\(Separated x) -> Separated (fmap (\(a, b) -> (b, a)) x))
    in iso swap swap

instance Bifunctor Separated where
  bimap f g (Separated x) =
    Separated (fmap (bimap f g) x)

instance Bifoldable Separated where
  bifoldr f g z (Separated x) =
    foldr (\(a, b) -> f a . g b) z x

instance Bitraversable Separated where
  bitraverse f g (Separated x) =
    Separated <$> traverse (\(a, b) -> (,) <$> f a <*> g b) x

-- | Map across a @Separated@ on the element values.
--
-- prop> fmap id (x :: Separated Int String) == x
--
-- prop> \a b -> fmap (+1) (a +: b +: emptySeparated) == a +: (1+b) +: emptySeparated
instance Functor (Separated a) where
  fmap =
    bimap id

instance Foldable (Separated a) where
  foldr f z (Separated xs) =
    foldr f z (fmap snd xs)

instance Traversable (Separated a) where
  traverse f (Separated xs) =
    Separated <$> traverse (traverse f) xs

-- | Applies functions with element values, using a zipping operation, appending
-- separators.
--
-- >>> (emptySeparated :: Separated [Int] (String -> [String])) <.> emptySeparated
-- []
--
-- >>> [1,2] +: (\s -> [s, reverse s, drop 1 s]) +: emptySeparated <.> [3,4,5] +: "abc" +: emptySeparated
-- [[1,2,3,4,5],["abc","cba","bc"]]
instance Semigroup a => Apply (Separated a) where
  (<.>) =
    separatedAp (<>)

-- | Applies functions with element values, using a zipping operation, appending
-- separators. The identity operation is an infinite list of the emptySeparated separator
-- and the given element value.
--
-- >>> (emptySeparated :: Separated [Int] (String -> [String])) <*> emptySeparated
-- []
--
-- >>> [1,2] +: (\s -> [s, reverse s, drop 1 s]) +: emptySeparated <*> [3,4,5] +: "abc" +: emptySeparated
-- [[1,2,3,4,5],["abc","cba","bc"]]
instance Monoid a => Applicative (Separated a) where    
  (<*>) =
    separatedAp mappend
  pure =
    Separated . repeat . (,) mempty

-- |
--
-- >>> show (emptySeparated :: Separated () ())
-- "[]"
--
-- >>> show ('x' +: (6 :: Int) +: emptySeparated)
-- "['x',6]"
instance (Show a, Show b) => Show (Separated a b) where
  show (Separated x) =
    showSeparated id x

-- |
--
-- >>> ('x' +: (6 :: Int) +: emptySeparated) <> ('y' +: 7 +: emptySeparated)
-- ['x',6,'y',7]
instance Semigroup (Separated a b) where
  Separated x <> Separated y =
    Separated (x <> y)    

-- |
--
-- >>> ('x' +: (6 :: Int) +: emptySeparated) `mappend` ('y' +: 7 +: emptySeparated)
-- ['x',6,'y',7]
instance Monoid (Separated a b) where
  mappend =
    (<>)
  mempty =
    Separated mempty

----

instance Bifunctor Separated1 where
  bimap f g (Separated1 a x) =
    Separated1 (f a) (bimap g f x)

instance Bifoldable Separated1 where
  bifoldr f g z (Separated1 a x) = 
    f a (bifoldr g f z x)

instance Bitraversable Separated1 where
  bitraverse f g (Separated1 a x) =
    Separated1 <$> f a <*> bitraverse g f x

-- | Map across a @Separated1@ on the separator values.
--
-- >>> fmap (+1) (set tailL (1 +: 'b' +: 2 +: 'c' +: emptySeparated) (singleSeparated 'a' :: Separated1 Char Int))
-- ['a',2,'b',3,'c']
--
-- prop> fmap id (x :: Separated1 Int String) == x
--
-- prop> fmap (+1) (singleSeparated x :: Separated1 Char Int) == singleSeparated x
instance Functor (Separated1 b) where
  fmap =
    bimap id

instance Foldable (Separated1 b) where
  foldr =
    bifoldr (flip const)

instance Traversable (Separated1 b) where
  traverse =
    bitraverse pure

-- | Applies functions with separator values, using a zipping operation,
-- appending elements.
--
-- >>> [1,2] +: reverse +: [3,4] +: emptySeparated <.> [5,6,7] +: "abc" +: [8] +: emptySeparated
-- [[1,2,5,6,7],"cba",[3,4,8]]
instance Semigroup b => Apply (Separated1 b) where
  (<.>) =
    separated1Ap (<>)

-- | Applies functions with separator values, using a zipping operation,
-- appending elements. The identity operation is an infinite list of the emptySeparated
-- element and the given separator value.
--
-- >>> [1,2] +: reverse +: [3,4] +: emptySeparated <*> [5,6,7] +: "abc" +: [8] +: emptySeparated
-- [[1,2,5,6,7],"cba",[3,4,8]]
instance Monoid b => Applicative (Separated1 b) where    
  (<*>) =
    separated1Ap mappend
  pure =
    Separated1 mempty . (swapped #) . pure

instance (Show b, Show a) => Show (Separated1 b a) where
  show (Separated1 a (Separated x)) =
    showSeparated (show a:) x

-- | The isomorphism that swaps elements with their separators.
--
-- >>> swapped # emptyPesarated
-- []
--
-- >>> swapped # ('x' -: 6 -: emptyPesarated)
-- [6,'x']
--
-- >>> emptyPesarated ^. swapped
-- []
--
-- >>> ('x' -: 6 -: emptyPesarated) ^. swapped
-- [6,'x']
instance Swapped Pesarated where
  swapped =
    _Wrapped . swapped . from _Wrapped

instance Bifunctor Pesarated where
  bimap f g (Pesarated x) =
    Pesarated (bimap g f x)

instance Bifoldable Pesarated where
  bifoldr f g z (Pesarated x) =
    bifoldr g f z x

instance Bitraversable Pesarated where
  bitraverse f g (Pesarated x) =
    Pesarated <$> bitraverse g f x

-- | Map across a @Pesarated@ on the element values.
--
-- prop> fmap id (x :: Pesarated Int String) == x
--
-- prop> \a b -> fmap (+1) (a -: b -: emptyPesarated) == (1+a) -: b -: emptyPesarated
instance Functor (Pesarated a) where
  fmap f (Pesarated x) =
    Pesarated (first f x)

instance Foldable (Pesarated b) where
  foldr =
    bifoldr (flip const)

instance Traversable (Pesarated b) where
  traverse =
    bitraverse pure


-- | Applies functions with element values, using a zipping operation, appending
-- separators.
--
-- >>> (emptyPesarated :: Pesarated [Int] (String -> [String])) <.> emptyPesarated
-- []
--
-- >>> (\s -> [s, reverse s, drop 1 s]) -: [1,2] -: emptyPesarated <.> "abc" -: [3,4,5] -: emptyPesarated
-- [["abc","cba","bc"],[1,2,3,4,5]]
instance Semigroup a => Apply (Pesarated a) where
  Pesarated f <.> Pesarated a =
    Pesarated ((swapped # f <.> swapped # a) ^. swapped)

-- | Applies functions with element values, using a zipping operation, appending
-- separators. The identity operation is an infinite list of the emptySeparated separator
-- and the given element value.
--
-- >>> (emptySeparated :: Separated [Int] (String -> [String])) <*> emptySeparated
-- []
--
-- >>> (\s -> [s, reverse s, drop 1 s]) -: [1,2] -: emptyPesarated <*> "abc" -: [3,4,5] -: emptyPesarated
-- [["abc","cba","bc"],[1,2,3,4,5]]
instance Monoid a => Applicative (Pesarated a) where
  Pesarated f <*> Pesarated a =
    Pesarated ((swapped # f <*> swapped # a) ^. swapped)
  pure a =
    Pesarated (pure a ^. swapped)

-- |
--
-- >>> show (emptyPesarated :: Pesarated () ())
-- "[]"
--
-- >>> show ('x' -: (6 :: Int) -: emptyPesarated)
-- "['x',6]"
instance (Show a, Show b) => Show (Pesarated b a) where
  show (Pesarated x) =
    show x

-- |
--
-- >>> ('x' -: (6 :: Int) -: emptyPesarated) <> ('y' -: 7 -: emptyPesarated)
-- ['x',6,'y',7]
instance Semigroup (Pesarated b a) where
  Pesarated a <> Pesarated b =
    Pesarated (a <> b)

-- |
--
-- >>> ('x' -: (6 :: Int) -: emptyPesarated) `mappend` ('y' -: 7 -: emptyPesarated)
-- ['x',6,'y',7]
instance Monoid (Pesarated b a) where
  Pesarated a `mappend` Pesarated b =
    Pesarated (a <> b)
  mempty =
    Pesarated mempty

instance Bifunctor Pesarated1 where
  bimap f g (Pesarated1 x) =
    Pesarated1 (bimap g f x)

instance Bifoldable Pesarated1 where
  bifoldr f g z (Pesarated1 x) =
    bifoldr g f z x

instance Bitraversable Pesarated1 where
  bitraverse f g (Pesarated1 x) =
    Pesarated1 <$> bitraverse g f x

-- | Map across a @Pesarated1@ on the separator values.
--
-- >>> fmap toUpper (set tailL (1 -: 'b' -: 2 -: 'c' -: empty) (singlePesarated 'z' :: Pesarated1 Int Char) :: Pesarated1 Int Char)
-- ['Z',1,'B',2,'C']
--
-- prop> fmap id (x :: Pesarated1 Int String) == x
--
-- prop> fmap (+1) (singlePesarated x :: Pesarated1 Char Int) == singlePesarated (x + 1)
instance Functor (Pesarated1 a) where
  fmap f (Pesarated1 x) =
    Pesarated1 (first f x)

instance Foldable (Pesarated1 a) where
  foldr =
    bifoldr (flip const)

instance Traversable (Pesarated1 a) where
  traverse =
    bitraverse pure

-- | Applies functions with separator values, using a zipping operation,
-- appending elements.
--
-- >>> id -: [1,2] -: reverse -: [3,4] -: emptyPesarated <.> "def" -: [5,6,7] -: "abc" -: [8] -: emptyPesarated
-- ["def",[1,2,5,6,7],"cba",[3,4,8]]
instance Semigroup a => Apply (Pesarated1 a) where
  f <.> a =
    pesarated1Ap (<>) f a

-- | Applies functions with separator values, using a zipping operation,
-- appending elements. The identity operation is an infinite list of the emptySeparated
-- element and the given separator value.
--
-- >>> id -: [1,2] -: reverse -: [3,4] -: emptyPesarated <*> "def" -: [5,6,7] -: "abc" -: [8] -: emptyPesarated
-- ["def",[1,2,5,6,7],"cba",[3,4,8]]
instance Monoid a => Applicative (Pesarated1 a) where
  f <*> a =
    pesarated1Ap mappend f a
  pure a =
    Pesarated1 (Separated1 a (pure a))

instance (Show a, Show b) => Show (Pesarated1 a b) where
  show (Pesarated1 x) =
    show x

---- not exported

showSeparated ::
 (Show a, Show s, Functor f) =>
 (f String -> [String])
 -> f (s, a)
 -> String
showSeparated f x =
  '[' : intercalate "," (f (fmap (\(s, a) -> show s <> "," <> show a) x)) <> "]"

separatedAp ::
  (s -> s -> s)
  -> Separated s (a -> b)
  -> Separated s a
  -> Separated s b
separatedAp opp (Separated f) (Separated a) =
    Separated (zipWith (\(s, f') (t, a') -> (s `opp` t, f' a')) f a)  

separated1Ap ::
  (a -> a -> a)
  -> Separated1 a (s -> t)
  -> Separated1 a s
  -> Separated1 a t
separated1Ap opp (Separated1 f (Separated fs)) (Separated1 a (Separated as)) =
    Separated1 (f `opp` a) (Separated (zipWith (\(s, f') (t, a') -> (s t, f' `opp` a')) fs as))

pesarated1Ap ::
  (a -> a -> a)
  -> Pesarated1 a (s -> t)
  -> Pesarated1 a s
  -> Pesarated1 a t
pesarated1Ap opp (Pesarated1 (Separated1 f (Separated fs))) (Pesarated1 (Separated1 a (Separated as))) =
  Pesarated1 (Separated1 (f a) (Separated (zipWith (\(s, f') (t, a') -> (s `opp` t, f' a')) fs as)))

-- | The isomorphism that shuffles the elements and separators one position.
--
-- >>> shift # ([], 6)
-- [6]
--
-- >>> shift # ([(5, 'x')], 6)
-- [5,'x',6]
--
-- >>> singleSeparated 6 ^. shift
-- ([],6)
--
-- >>> (5 +: 'x' +: singleSeparated 6) ^. shift
-- ([(5,'x')],6)
shift ::
  Iso (Separated1 a s) (Separated1 b t) ([(a, s)], a) ([(b, t)], b)
shift =
  let shiftR ([], a) =
        Separated1 a (Separated [])
      shiftR ((b, s):r, a) =
        let Separated1 z' (Separated w) = shiftR (r, b)
        in Separated1 z' (Separated ((s, a) : w))
      shiftL (Separated1 s' (Separated [])) =
        ([], s')
      shiftL (Separated1 s' (Separated ((a, t') : r))) =
        let (w, z) = shiftL (Separated1 t' (Separated r))
        in ((s', a) : w, z)
  in iso shiftL shiftR

makeWrapped ''Separated
makeWrapped ''Pesarated
makeWrapped ''Pesarated1
