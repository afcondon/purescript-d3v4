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
  -- , text
  -- , tween
  ) where

import D3.Base (D3Element, D3, Eff)
import D3.Interpolator (Index, D3TweenFn, D3TweenTarget, D3TweenFnUncurried, D3TweenTargetUncurried)
import D3.Selection (Selection)
import Data.Function.Eff (mkEffFn3, EffFn3, EffFn2, EffFn1, runEffFn3, runEffFn2, runEffFn1)
import Prelude (unit)
import Unsafe.Coerce (unsafeCoerce)

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
foreign import attrIFn           :: ∀ d v eff. EffFn3 (d3::D3|eff) String (EffFn3 (d3::D3|eff) d Index D3Element v) (Transition d) (Transition d)
foreign import styleIFn          :: ∀ d v eff. EffFn3 (d3::D3|eff) String (EffFn3 (d3::D3|eff) d Index D3Element v) (Transition d) (Transition d)
foreign import styleTweenFn      :: ∀ d v eff. EffFn3 (d3::D3|eff) String (EffFn3 (d3::D3|eff) d Index D3Element v) (Transition d) (Transition d)

data AttrInterpolator d v =
      Target v  -- straightforward target final value to tween to using built-in interpolators
    | TweenTarget (D3TweenTarget v d) -- function which is called a single time to get a target final value
    | TweenFn     (D3TweenFn     v d) -- function which is called once to generate a function which is then
                                      -- called every tween frame to generate a value

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
tAttr name (Target v)      = runEffFn3 attrFn       name v
tAttr name _ = unsafeCoerce unit
-- tAttr name (TweenTarget f) = runEffFn3 attrIFn      name (mkEffFn3 f)
-- tAttr name (TweenFn f)     = runEffFn3 styleTweenFn name (mkEffFn3 f)

tStyle :: ∀ d v eff. String -> AttrInterpolator d v -> Transition d -> Eff (d3::D3|eff) (Transition d)
tStyle name (Target v)      = runEffFn3 styleFn      name v
tStyle name (TweenTarget f) = runEffFn3 styleIFn     name (mkEffFn3 f)
tStyle name _ = unsafeCoerce unit
-- tStyle name (TweenFn f)     = runEffFn3 styleTweenFn name (mkEffFn3 f)
