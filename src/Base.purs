module D3.Base
  ( module Control.Monad.Eff
  , D3
  , D3Element
  , Index
  , Nodes
  , (..)
  , (...)
  , flipply
  , PredicateFn
  , PredicateB
  , PredicateS
  , PredicateN
  , theHorror
  ) where

import Control.Monad.Eff (Eff)
import Data.Foreign.Null (writeNull)
import Prelude (bind, flip, ($))
import Unsafe.Coerce (unsafeCoerce)

-- || FFI for D3
foreign import data D3 :: !
foreign import data D3Element :: *

type D3Eff a = ∀ e. Eff (d3 :: D3 | e) a

type Index = Number
type Nodes = Array D3Element

theHorror :: ∀ t0. t0
theHorror = unsafeCoerce writeNull

-- Syntactic sugar to make chained monadic statements look similar to the
-- "fluid interface" style of chained method calls in JavaScript
infixl 4 bind as ..
-- Reversed function application, useful for applying extended monadic chains
-- to already-obtained values
flipply = flip ($)
infixl 4 flipply as ...   -- (...) = flip ($)

-- for selection.classed and selection.attr:
type PredicateFn r d  = (d -> Number -> (Array D3Element) -> D3Element -> r)
type PredicateB    d  = (d -> Number -> (Array D3Element) -> D3Element -> Boolean)
type PredicateS    d  = (d -> Number -> (Array D3Element) -> D3Element -> String)
type PredicateN    d  = (d -> Number -> (Array D3Element) -> D3Element -> Number)
