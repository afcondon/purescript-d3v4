module Data.Monoid.Additive where

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
import Data.Monoid (class Monoid)
import Data.Ord (class Ord, compare)
import Data.Semigroup (class Semigroup, (<>))
import Data.Semiring (class Semiring, (+), zero)
import Data.Show (class Show, show)

-- | Monoid and semigroup for semirings under addition.
-- |
-- | ``` purescript
-- | Additive x <> Additive y == Additive (x + y)
-- | mempty :: Additive _ == Additive zero
-- | ```
newtype Additive a = Additive a

runAdditive :: forall a. Additive a -> a
runAdditive (Additive x) = x

instance eqAdditive :: Eq a => Eq (Additive a) where
  eq (Additive x) (Additive y) = x == y

instance ordAdditive :: Ord a => Ord (Additive a) where
  compare (Additive x) (Additive y) = compare x y

instance boundedAdditive :: Bounded a => Bounded (Additive a) where
  top = Additive top
  bottom = Additive bottom

instance functorAdditive :: Functor Additive where
  map f (Additive x) = Additive (f x)

instance invariantAdditive :: Invariant Additive where
  imap f _ (Additive x) = Additive (f x)

instance applyAdditive :: Apply Additive where
  apply (Additive f) (Additive x) = Additive (f x)

instance applicativeAdditive :: Applicative Additive where
  pure = Additive

instance bindAdditive :: Bind Additive where
  bind (Additive x) f = f x

instance monadAdditive :: Monad Additive

instance extendAdditive :: Extend Additive where
  extend f x = Additive (f x)

instance comonadAdditive :: Comonad Additive where
  extract = runAdditive

instance showAdditive :: Show a => Show (Additive a) where
  show (Additive a) = "(Additive " <> show a <> ")"

instance semigroupAdditive :: Semiring a => Semigroup (Additive a) where
  append (Additive a) (Additive b) = Additive (a + b)

instance monoidAdditive :: Semiring a => Monoid (Additive a) where
  mempty = Additive zero
