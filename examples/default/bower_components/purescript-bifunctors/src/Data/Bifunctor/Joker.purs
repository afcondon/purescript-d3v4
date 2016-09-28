module Data.Bifunctor.Joker where

import Control.Applicative (class Applicative, pure)
import Control.Apply (class Apply, (<*>))
import Control.Biapplicative (class Biapplicative)
import Control.Biapply (class Biapply)
import Control.Semigroupoid ((<<<))

import Data.Bifunctor (class Bifunctor)
import Data.Functor (class Functor, map)

-- | Make a `Functor` over the second argument of a `Bifunctor`
newtype Joker g a b = Joker (g b)

-- | Remove the `Joker` constructor.
runJoker :: forall g a b. Joker g a b -> g b
runJoker (Joker gb) = gb

instance functorJoker :: (Functor g) => Functor (Joker g a) where
  map g = Joker <<< map g <<< runJoker

instance bifunctorJoker :: (Functor g) => Bifunctor (Joker g) where
  bimap _ g = Joker <<< map g <<< runJoker

instance biapplyJoker :: (Apply g) => Biapply (Joker g) where
  biapply (Joker fg) (Joker xy) = Joker (fg <*> xy)

instance biapplicativeJoker :: (Applicative g) => Biapplicative (Joker g) where
  bipure _ b = Joker (pure b)
