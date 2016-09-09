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
  , tStyleTween
  -- , text
  -- , tween
  ) where

import D3.Base (D3Element, D3, Eff)
import D3.Interpolator (Time, Interpolator)
import D3.Selection (Selection)
import Data.Function.Eff (EffFn3, EffFn2, EffFn1, runEffFn3, runEffFn2, runEffFn1)

foreign import data Transition :: * -> *

foreign import d3TransitionFn    :: ∀ d eff.   EffFn1 (d3::D3|eff) String                                     (Transition d)
foreign import transitionFn      :: ∀ d eff.   EffFn1 (d3::D3|eff)                             (Selection d)  (Transition d)
foreign import namedTransitionFn :: ∀ d eff.   EffFn2 (d3::D3|eff) String                      (Selection d)  (Transition d)
-- when using a saved transition you're going to be using it with different types of selections to this function will
-- morph it from type x to type d, the type of the selection to which it is being applied
foreign import savedTransitionFn :: ∀ d x eff. EffFn2 (d3::D3|eff) (Transition x)              (Selection d)  (Transition d)
foreign import durationFn        :: ∀ d eff.   EffFn2 (d3::D3|eff) Number                      (Transition d) (Transition d)
foreign import attrFn            :: ∀ d v eff. EffFn3 (d3::D3|eff) String v                    (Transition d) (Transition d)
foreign import styleFn           :: ∀ d v eff. EffFn3 (d3::D3|eff) String v                    (Transition d) (Transition d)
foreign import attrIFn           :: ∀ d v eff. EffFn3 (d3::D3|eff) String (Interpolator v d)   (Transition d) (Transition d)
foreign import styleIFn          :: ∀ d v eff. EffFn3 (d3::D3|eff) String (Interpolator v d)   (Transition d) (Transition d)
foreign import styleTweenFn      :: ∀ d v eff. EffFn3 (d3::D3|eff) String
                                                                  (D3EffInterpolator (d3::D3|eff) d Number D3Element (Time -> v))
                                                                  (Transition d)
                                                                  (Transition d)

foreign import data D3EffInterpolator ::                                                                 # ! -> *  -> * -> *   -> *      -> *
foreign import mkInterpolator    :: ∀ d v eff. (d -> Number -> D3Element -> (Time -> v))  -> D3EffInterpolator eff    d    Number D3Element (Time -> v)

type InitialFn    v d = (d -> Number -> D3Element -> v)
data AttrInterpolator d v = Target v
                          | Interpolate (Interpolator v d)

d3Transition :: ∀ d eff. String                                     -> Eff (d3::D3|eff) (Transition d)
d3Transition name           = runEffFn1 d3TransitionFn name

makeTransition :: ∀ d eff.                             Selection d  -> Eff (d3::D3|eff) (Transition d)
makeTransition              = runEffFn1 transitionFn

namedTransition :: ∀ d eff. String                  -> Selection d  -> Eff (d3::D3|eff) (Transition d)
namedTransition name        = runEffFn2 namedTransitionFn name

savedTransition :: ∀ d x eff. (Transition x)        -> Selection d  -> Eff (d3::D3|eff) (Transition d)
savedTransition name        = runEffFn2 savedTransitionFn name

duration :: ∀ d eff. Number                         -> Transition d -> Eff (d3::D3|eff) (Transition d)
duration t                  = runEffFn2 durationFn t

tAttr :: ∀ d v eff. String -> AttrInterpolator d v  -> Transition d -> Eff (d3::D3|eff) (Transition d)
tAttr name (Target v)       = runEffFn3 attrFn  name v
tAttr name (Interpolate f)  = runEffFn3 attrIFn name f

tStyle :: ∀ d v eff. String -> AttrInterpolator d v -> Transition d -> Eff (d3::D3|eff) (Transition d)
tStyle name (Target v)      = runEffFn3 styleFn  name v
tStyle name (Interpolate f) = runEffFn3 styleIFn name f

tStyleTween :: ∀ d v eff. String
                       -> (d -> Number -> D3Element -> (Time -> v))
                       -> Transition d
                       -> Eff (d3::D3|eff) (Transition d)
tStyleTween name f = runEffFn3 styleTweenFn name (mkInterpolator f)
