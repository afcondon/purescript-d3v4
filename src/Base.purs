module D3.Base
  ( module Control.Monad.Eff
  , D3
  , D3Element
  , Index
  , Nodes
  , (..)
  , (...)
  , flipply
  , DIEfn
  , DINEfn
  , PredicateFn
  , PredicateB
  , PredicateS
  , PredicateN
  , InitialFn
  , InterpolatorFn
  , theHorror
  ) where

import Control.Monad.Eff (Eff)
import Data.Foreign.Null (writeNull)
import Prelude (bind, flip, ($))
import Unsafe.Coerce (unsafeCoerce)

import D3.Interpolator (Interpolator)

-- || FFI for D3
foreign import data D3 :: !
foreign import data D3Element :: *

type D3Eff a = forall e. Eff (d3 :: D3 | e) a

type Index = Number
type Nodes = Array D3Element

theHorror :: forall t0. t0
theHorror = unsafeCoerce writeNull

-- Syntactic sugar to make chained monadic statements look similar to the
-- "fluid interface" style of chained method calls in JavaScript
infixl 4 bind as ..
-- Reversed function application, useful for applying extended monadic chains
-- to already-obtained values
flipply = flip ($)
infixl 4 flipply as ...   -- (...) = flip ($)

-- make some of the common patterns of assist functions a little less unwieldy with a type alias
-- Datum Index Nodes Element
type DINEfn r d  = (d -> Number -> (Array D3Element) -> D3Element -> r)
-- Datum Index       Element
type DIEfn  r d  = (d -> Number                      -> D3Element -> r)
-- some specializations of the above for predicate functions that return a Boolean, String or Number

-- for selection.classed and selection.attr:
type PredicateFn r d  = DINEfn r d
type PredicateB d     = DINEfn Boolean d
type PredicateS d     = DINEfn String d
type PredicateN d     = DINEfn Number d

-- for transition.attr:
type InitialFn v d      = DIEfn v d
type InterpolatorFn v d = DIEfn (Interpolator v) d
