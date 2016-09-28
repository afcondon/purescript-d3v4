module Data.Monoid.Multiplicative where

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
import Data.Semiring (class Semiring, (*), one)
import Data.Show (class Show, show)

-- | Monoid and semigroup for semirings under multiplication.
-- |
-- | ``` purescript
-- | Multiplicative x <> Multiplicative y == Multiplicative (x * y)
-- | mempty :: Multiplicative _ == Multiplicative one
-- | ```
newtype Multiplicative a = Multiplicative a

runMultiplicative :: forall a. Multiplicative a -> a
runMultiplicative (Multiplicative x) = x

instance eqMultiplicative :: (Eq a) => Eq (Multiplicative a) where
  eq (Multiplicative x) (Multiplicative y) = x == y

instance ordMultiplicative :: (Ord a) => Ord (Multiplicative a) where
  compare (Multiplicative x) (Multiplicative y) = compare x y

instance boundedMultiplicative :: Bounded a => Bounded (Multiplicative a) where
  top = Multiplicative top
  bottom = Multiplicative bottom

instance functorMultiplicative :: Functor Multiplicative where
  map f (Multiplicative x) = Multiplicative (f x)

instance invariantMultiplicative :: Invariant Multiplicative where
  imap f _ (Multiplicative x) = Multiplicative (f x)

instance applyMultiplicative :: Apply Multiplicative where
  apply (Multiplicative f) (Multiplicative x) = Multiplicative (f x)

instance applicativeMultiplicative :: Applicative Multiplicative where
  pure = Multiplicative

instance bindMultiplicative :: Bind Multiplicative where
  bind (Multiplicative x) f = f x

instance monadMultiplicative :: Monad Multiplicative

instance extendMultiplicative :: Extend Multiplicative where
  extend f x = Multiplicative (f x)

instance comonadMultiplicative :: Comonad Multiplicative where
  extract = runMultiplicative

instance showMultiplicative :: (Show a) => Show (Multiplicative a) where
  show (Multiplicative a) = "(Multiplicative " <> show a <> ")"

instance semigroupMultiplicative :: (Semiring a) => Semigroup (Multiplicative a) where
  append (Multiplicative a) (Multiplicative b) = Multiplicative (a * b)

instance monoidMultiplicative :: (Semiring a) => Monoid (Multiplicative a) where
  mempty = Multiplicative one
