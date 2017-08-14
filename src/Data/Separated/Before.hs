{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}

module Data.Separated.Before
  ( Before(..)
  , before
  )
  where

import Prelude (Show, uncurry)
import Control.Applicative
import Control.Lens
import Data.Bifoldable
import Data.Bitraversable
import Data.Deriving
import Data.Eq
import Data.Foldable
import Data.Functor
import Data.Monoid
import Data.Ord

-- | An 's' that comes before an 'a'
data Before s a = Before s a
  deriving (Eq, Foldable, Functor, Ord, Traversable, Show)

deriveEq1 ''Before
deriveShow1 ''Before

instance Bifunctor Before where
  bimap f g (Before s a) = Before (f s) (g a)

instance Bifoldable Before where
  bifoldMap f g (Before s a) = f s <> g a

instance Bitraversable Before where
  bitraverse f g (Before s a) = Before <$> f s <*> g a

-- | 'Before s a' is isomorphic to '(s, a)'
before :: Iso (s, a) (t, b) (Before s a) (Before t b) 
before = iso (uncurry Before) (\(Before s a) -> (s, a))
