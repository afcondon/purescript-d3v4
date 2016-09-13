module D3.Base
  ( module Control.Monad.Eff
  , D3
  , D3Element
  , Index
  , Nodes
  , (..)
  , (...)
  , PredicateFn
  , PredicateB
  , PredicateS
  , PredicateN
  , theHorror
  ) where

import Control.Monad.Eff (Eff)
import Data.Foreign.Null (writeNull)
import Data.Function (applyFlipped)
import Prelude (bind)
import Unsafe.Coerce (unsafeCoerce)

-- || FFI for D3
foreign import data D3 :: !
foreign import data D3Element :: *

type D3Eff a = ∀ e. Eff (d3 :: D3 | e) a

type Index = Number
type Nodes = Array D3Element

theHorror :: ∀ t0. t0
theHorror = unsafeCoerce writeNull

foreign import approxZero :: Number

-- | These next two operators are really key to making this DSL look like D3 in JavaScript
-- | All respect to pelotom for cooking them up in the original purescript-d3!

-- Syntactic sugar to make chained monadic statements look similar to the
-- "fluid interface" style of chained method calls in JavaScript
infixl 4 bind as ..
-- Reversed function application, useful for applying extended monadic chains
-- to already-obtained values
infixl 4 applyFlipped as ...

-- for selection.classed and selection.attr:
type PredicateFn r d  = (d -> Number -> (Array D3Element) -> D3Element -> r)
type PredicateB    d  = (d -> Number -> (Array D3Element) -> D3Element -> Boolean)
type PredicateS    d  = (d -> Number -> (Array D3Element) -> D3Element -> String)
type PredicateN    d  = (d -> Number -> (Array D3Element) -> D3Element -> Number)
