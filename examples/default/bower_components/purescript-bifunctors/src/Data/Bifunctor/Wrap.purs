module Data.Bifunctor.Wrap where

import Control.Biapplicative (class Biapplicative, bipure)
import Control.Biapply (class Biapply, (<<*>>))
import Control.Semigroupoid ((<<<))

import Data.Bifunctor (class Bifunctor, bimap, rmap)
import Data.Functor (class Functor)

-- | Provides a `Functor` over the second argument of a `Bifunctor`.
newtype Wrap p a b = Wrap (p a b)

-- | Remove the `Wrap` constructor.
unwrap :: forall p a b. Wrap p a b -> p a b
unwrap (Wrap pab) = pab

instance bifunctorWrap :: Bifunctor p => Bifunctor (Wrap p) where
  bimap f g = Wrap <<< bimap f g <<< unwrap

instance functorWrap :: Bifunctor p => Functor (Wrap p a) where
  map f = Wrap <<< rmap f <<< unwrap

instance biapplyWrap :: Biapply p => Biapply (Wrap p) where
  biapply (Wrap fg) (Wrap xy) = Wrap (fg <<*>> xy)

instance biapplicativeWrap :: Biapplicative p => Biapplicative (Wrap p) where
  bipure a b = Wrap (bipure a b)
