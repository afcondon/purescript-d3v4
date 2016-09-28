module Data.Monoid.Dual where

import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Control.Monad (class Monad)

import Data.Bounded (class Bounded, top, bottom)
import Data.Eq (class Eq, (==))
import Data.Functor (class Functor)
import Data.Functor.Invariant (class Invariant)
import Data.Monoid (class Monoid, mempty)
import Data.Ord (class Ord, compare)
import Data.Semigroup (class Semigroup, (<>))
import Data.Show (class Show, show)

-- | The dual of a monoid.
-- |
-- | ``` purescript
-- | Dual x <> Dual y == Dual (y <> x)
-- | mempty :: Dual _ == Dual mempty
-- | ```
newtype Dual a = Dual a

runDual :: forall a. Dual a -> a
runDual (Dual x) = x

instance eqDual :: Eq a => Eq (Dual a) where
  eq (Dual x) (Dual y) = x == y

instance ordDual :: Ord a => Ord (Dual a) where
  compare (Dual x) (Dual y) = compare x y

instance boundedDual :: Bounded a => Bounded (Dual a) where
  top = Dual top
  bottom = Dual bottom

instance functorDual :: Functor Dual where
  map f (Dual x) = Dual (f x)

instance invariantDual :: Invariant Dual where
  imap f _ (Dual x) = Dual (f x)

instance applyDual :: Apply Dual where
  apply (Dual f) (Dual x) = Dual (f x)

instance applicativeDual :: Applicative Dual where
  pure = Dual

instance bindDual :: Bind Dual where
  bind (Dual x) f = f x

instance monadDual :: Monad Dual

instance extendDual :: Extend Dual where
  extend f x = Dual (f x)

instance comonadDual :: Comonad Dual where
  extract = runDual

instance showDual :: Show a => Show (Dual a) where
  show (Dual a) = "(Dual " <> show a <> ")"

instance semigroupDual :: Semigroup a => Semigroup (Dual a) where
  append (Dual x) (Dual y) = Dual (y <> x)

instance monoidDual :: Monoid a => Monoid (Dual a) where
  mempty = Dual mempty
