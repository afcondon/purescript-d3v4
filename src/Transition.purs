module D3.Transitions
  ( Transition          -- Types
  , AttrInterpolator(..)
  , d3Transition
  -- , addTransition
  , tAttr
  -- , attrTween
  -- , call
  -- , delay
  , duration
  -- , each
  -- , ease
  -- , empty
  -- , filter
  -- , merge
  , makeTransition
  , namedTransition
  , savedTransition
  -- , node
  -- , nodes
  -- , on
  -- , remove
  -- , select
  -- , selectAll
  -- , selection
  -- , size
  , tStyle
  -- , styleTween
  -- , text
  -- , tween
  ) where

import D3.Base (InterpolatorFn, D3, Eff)
import D3.Selection (Selection)
import Data.Function.Eff (runEffFn3, EffFn3, EffFn2, runEffFn2, EffFn1, runEffFn1)

foreign import data Transition :: * -> *

foreign import d3TransitionFn    :: forall d eff.   EffFn1 (d3::D3|eff) String                            (Transition d)
foreign import transitionFn      :: forall d eff.   EffFn1 (d3::D3|eff)                    (Selection d)  (Transition d)
foreign import namedTransitionFn :: forall d eff.   EffFn2 (d3::D3|eff) String             (Selection d)  (Transition d)
-- when using a saved transition you're going to be using it with different types of selections to this function will
-- morph it from type x to type d, the type of the selection to which it is being applied
foreign import savedTransitionFn :: forall d x eff.   EffFn2 (d3::D3|eff) (Transition x)     (Selection d)  (Transition d)
foreign import durationFn :: forall d eff. EffFn2 (d3::D3|eff) Number                        (Transition d) (Transition d)
foreign import attrFn     :: forall d v eff. EffFn3 (d3::D3|eff) String v                    (Transition d) (Transition d)
foreign import styleFn    :: forall d v eff. EffFn3 (d3::D3|eff) String v                    (Transition d) (Transition d)
foreign import attrIFn    :: forall d v eff. EffFn3 (d3::D3|eff) String (InterpolatorFn v d) (Transition d) (Transition d)
foreign import styleIFn   :: forall d v eff. EffFn3 (d3::D3|eff) String (InterpolatorFn v d) (Transition d) (Transition d)

data AttrInterpolator d v = Start v
                          | Interpolate (InterpolatorFn v d)

d3Transition :: forall d eff. String                                  -> Eff (d3::D3|eff) (Transition d)
d3Transition name     = runEffFn1 d3TransitionFn name

makeTransition :: forall d eff.                          Selection d  -> Eff (d3::D3|eff) (Transition d)
makeTransition        = runEffFn1 transitionFn

namedTransition :: forall d eff. String               -> Selection d  -> Eff (d3::D3|eff) (Transition d)
namedTransition name  = runEffFn2 namedTransitionFn name

savedTransition :: forall d x eff. (Transition x)       -> Selection d  -> Eff (d3::D3|eff) (Transition d)
savedTransition name  = runEffFn2 savedTransitionFn name

duration :: forall d eff. Number                      -> Transition d -> Eff (d3::D3|eff) (Transition d)
duration t            = runEffFn2 durationFn t

tAttr :: forall d v eff. String -> AttrInterpolator d v -> Transition d -> Eff (d3::D3|eff) (Transition d)
tAttr name (Start v)       = runEffFn3 attrFn  name v
tAttr name (Interpolate f) = runEffFn3 attrIFn name f

tStyle :: forall d v eff. String -> AttrInterpolator d v -> Transition d -> Eff (d3::D3|eff) (Transition d)
tStyle name (Start v)       = runEffFn3 styleFn  name v
tStyle name (Interpolate f) = runEffFn3 styleIFn name f
