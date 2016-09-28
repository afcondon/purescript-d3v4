module Data.Bifunctor.Flip where

import Control.Biapplicative (class Biapplicative, bipure)
import Control.Biapply (class Biapply, (<<*>>))
import Control.Semigroupoid ((<<<))

import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Functor (class Functor)

-- | Flips the order of the type arguments of a `Bifunctor`.
newtype Flip p a b = Flip (p b a)

-- | Remove the `Flip` constructor.
runFlip :: forall p a b. Flip p a b -> p b a
runFlip (Flip pba) = pba

instance functorFlip :: Bifunctor p => Functor (Flip p a) where
  map f = Flip <<< lmap f <<< runFlip

instance bifunctorFlip :: Bifunctor p => Bifunctor (Flip p) where
  bimap f g = Flip <<< bimap g f <<< runFlip

instance biapplyFlip :: Biapply p => Biapply (Flip p) where
  biapply (Flip fg) (Flip xy) = Flip (fg <<*>> xy)

instance biapplicativeFlip :: Biapplicative p => Biapplicative (Flip p) where
  bipure a b = Flip (bipure b a)
