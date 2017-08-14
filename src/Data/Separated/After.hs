{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}

module Data.Separated.After
  ( After(..)
  , after
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

-- | An 's' that comes after an 'a'
data After s a = After a s
  deriving (Eq, Foldable, Functor, Ord, Traversable, Show)

deriveEq1 ''After
deriveShow1 ''After

instance Bifunctor After where
  bimap f g (After a s) = After (g a) (f s)

instance Bifoldable After where
  bifoldMap f g (After a s) = g a <> f s

instance Bitraversable After where
  bitraverse f g (After a s) = After <$> g a <*> f s

-- | 'After s a' is isomorphic to '(a, s)'
after :: Iso (a, s) (b, t) (After s a) (After t b)
after = iso (uncurry After) (\(After a s) -> (a, s))
