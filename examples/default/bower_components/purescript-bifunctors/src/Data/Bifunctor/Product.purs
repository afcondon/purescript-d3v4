module Data.Bifunctor.Product where

import Control.Biapplicative (class Biapplicative, bipure)
import Control.Biapply (class Biapply, biapply)

import Data.Bifunctor (class Bifunctor, bimap)

-- | The product of two `Bifunctor`s.
data Product f g a b = Pair (f a b) (g a b)

instance bifunctorProduct :: (Bifunctor f, Bifunctor g) => Bifunctor (Product f g) where
  bimap f g (Pair x y) = Pair (bimap f g x) (bimap f g y)

instance biapplyProduct :: (Biapply f, Biapply g) => Biapply (Product f g) where
  biapply (Pair w x) (Pair y z) = Pair (biapply w y) (biapply x z)

instance biapplicativeProduct :: (Biapplicative f, Biapplicative g) => Biapplicative (Product f g) where
  bipure a b = Pair (bipure a b) (bipure a b)
