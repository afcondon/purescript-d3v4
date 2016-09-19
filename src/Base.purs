module D3.Base
  ( module Control.Monad.Eff
  , D3
  , D3Element
  , Point
  , D3SetWithIndex
  , AttrSetter(..)
  , ClassSetter(..)
  , DataBind(..)
  , Filter(..)
  , PolyValue(..)
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
type Point = { x :: Number, y :: Number }

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
type PredicateFn  r d   = ∀ eff. (d -> Number -> (Array D3Element) -> D3Element -> Eff (d3::D3|eff) r)
type PredicateB     d   = ∀ eff. (d -> Number -> (Array D3Element) -> D3Element -> Eff (d3::D3|eff) Boolean)
type PredicateS     d   = ∀ eff. (d -> Number -> (Array D3Element) -> D3Element -> Eff (d3::D3|eff) String)
type PredicateN     d   = ∀ eff. (d -> Number -> (Array D3Element) -> D3Element -> Eff (d3::D3|eff) Number)
type D3SetWithIndex d v = ∀ eff. (d -> Index -> Eff (d3::D3|eff) v)

-- | ADT used to wrap those polymorphic calls in D3 which take either
--      a value, or...
--      a function to get a value from the datum, or...
--      a function to get a value from the datum and its index
data DataBind d k = Data (Array d)
                  | Keyed (Array d) (d -> k)

data PolyValue d v  = Value v
                    | SetByIndex (D3SetWithIndex d v)

data Filter d       = Selector  String
                    | Predicate (d -> Boolean)

data ClassSetter  d = SetAll Boolean
                    | SetSome (PredicateB d)

data AttrSetter v d = SetAttr v
                    | AttrFn (PredicateFn v d)   -- rename both data ctor and Type here TODO
