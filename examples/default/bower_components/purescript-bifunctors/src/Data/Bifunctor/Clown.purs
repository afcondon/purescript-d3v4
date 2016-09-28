module Data.Bifunctor.Clown where

import Control.Applicative (class Applicative, pure)
import Control.Apply (class Apply, (<*>))
import Control.Biapplicative (class Biapplicative)
import Control.Biapply (class Biapply)
import Control.Semigroupoid ((<<<))

import Data.Bifunctor (class Bifunctor)
import Data.Functor (class Functor, map)

-- | Make a `Functor` over the first argument of a `Bifunctor`
newtype Clown f a b = Clown (f a)

-- | Remove the `Clown` constructor.
runClown :: forall f a b. Clown f a b -> f a
runClown (Clown fa) = fa

instance bifunctorClown :: Functor f => Bifunctor (Clown f) where
  bimap f _ = Clown <<< map f <<< runClown

instance functorClown :: Functor (Clown f a) where
  map _ = Clown <<< runClown

instance biapplyClown :: Apply f => Biapply (Clown f) where
  biapply (Clown fg) (Clown xy) = Clown (fg <*> xy)

instance biapplicativeClown :: Applicative f => Biapplicative (Clown f) where
  bipure a _ = Clown (pure a)
