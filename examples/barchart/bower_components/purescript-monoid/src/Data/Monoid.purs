module Data.Monoid
  ( class Monoid, mempty
  , module Data.Semigroup
  ) where

import Data.Function (const)
import Data.Semigroup (class Semigroup, append, (<>))
import Data.Unit (Unit, unit)

-- | A `Monoid` is a `Semigroup` with a value `mempty`, which is both a
-- | left and right unit for the associative operation `<>`:
-- |
-- | ```text
-- | forall x. mempty <> x = x <> mempty = x
-- | ```
-- |
-- | `Monoid`s are commonly used as the result of fold operations, where
-- | `<>` is used to combine individual results, and `mempty` gives the result
-- | of folding an empty collection of elements.
class Semigroup m <= Monoid m where
  mempty :: m

instance monoidUnit :: Monoid Unit where
  mempty = unit

instance monoidFn :: Monoid b => Monoid (a -> b) where
  mempty = const mempty

instance monoidString :: Monoid String where
  mempty = ""

instance monoidArray :: Monoid (Array a) where
  mempty = []
