module Data.Bifunctor.Join where

import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Biapplicative (class Biapplicative, bipure)
import Control.Biapply (class Biapply, (<<*>>))
import Control.Semigroupoid ((<<<))

import Data.Bifunctor (class Bifunctor, bimap)
import Data.Functor (class Functor, (<$>))

-- | Turns a `Bifunctor` into a `Functor` by equating the two type arguments.
newtype Join p a = Join (p a a)

-- | Remove the `Join` constructor.
runJoin :: forall p a. Join p a -> p a a
runJoin (Join paa) = paa

instance bifunctorJoin :: Bifunctor p => Functor (Join p) where
  map f = Join <$> bimap f f <<< runJoin

instance biapplyJoin :: Biapply p => Apply (Join p) where
  apply (Join f) (Join a) = Join (f <<*>> a)

instance biapplicativeJoin :: Biapplicative p => Applicative (Join p) where
  pure a = Join (bipure a a)
