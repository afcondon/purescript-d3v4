-- | This module provides a type class for _unfoldable functors_, i.e.
-- | functors which support an `unfoldr` operation.
-- |
-- | This allows us to unify various operations on arrays, lists,
-- | sequences, etc.

module Data.Unfoldable
  ( class Unfoldable, unfoldr
  , replicate
  , replicateA
  , none
  , singleton
  ) where

import Prelude

import Data.Maybe (Maybe(..), isNothing, fromJust)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..), fst, snd)

import Partial.Unsafe (unsafePartial)

-- | This class identifies data structures which can be _unfolded_,
-- | generalizing `unfoldr` on arrays.
-- |
-- | The generating function `f` in `unfoldr f` in understood as follows:
-- |
-- | - If `f b` is `Nothing`, then `unfoldr f b` should be empty.
-- | - If `f b` is `Just (Tuple a b1)`, then `unfoldr f b` should consist of `a`
-- |   appended to the result of `unfoldr f b1`.
class Unfoldable t where
  unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> t a

instance unfoldableArray :: Unfoldable Array where
  unfoldr = unfoldrArrayImpl isNothing (unsafePartial fromJust) fst snd

foreign import unfoldrArrayImpl
  :: forall a b
   . (forall x. Maybe x -> Boolean)
  -> (forall x. Maybe x -> x)
  -> (forall x y. Tuple x y -> x)
  -> (forall x y. Tuple x y -> y)
  -> (b -> Maybe (Tuple a b))
  -> b
  -> Array a

-- | Replicate a value some natural number of times.
-- | For example:
-- |
-- | ~~~ purescript
-- | replicate 2 "foo" == ["foo", "foo"] :: Array String
-- | ~~~
replicate :: forall f a. Unfoldable f => Int -> a -> f a
replicate n v = unfoldr step n
  where
    step :: Int -> Maybe (Tuple a Int)
    step i =
      if i <= 0 then Nothing
      else Just (Tuple v (i - 1))

-- | Perform an Applicative action `n` times, and accumulate all the results.
replicateA
  :: forall m f a
   . (Applicative m, Unfoldable f, Traversable f)
  => Int
  -> m a
  -> m (f a)
replicateA n m = sequence (replicate n m)

-- | The container with no elements - unfolded with zero iterations.
-- | For example:
-- |
-- | ~~~ purescript
-- | none == [] :: forall a. Array a
-- | ~~~
none :: forall f a. Unfoldable f => f a
none = unfoldr (const Nothing) unit

-- | Contain a single value.
-- | For example:
-- |
-- | ~~~ purescript
-- | singleton "foo" == ["foo"] :: Array String
-- | ~~~
singleton :: forall f a. Unfoldable f => a -> f a
singleton = replicate 1
