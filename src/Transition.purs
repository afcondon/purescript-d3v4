module D3.Transitions
  ( Transition          -- Types
  , D3DelayFn
  , DelayValue(..)
  , AttrInterpolator(..)
  , TransitionName(..)
  , d3Transition
  , addTransition
  , tAttr
  -- , call
  , delay
  , duration
  -- , each
  -- , ease
  , tEmpty
  , tFilter
  , tMerge
  , makeTransition
  , namedTransition
  , savedTransition
  , tNode       -- implements D3 transition.node()
  , tNodes      -- implements D3 transition.nodes()
  -- , on
  , tRemove
  , tSelect
  , tSelectAll
  , makeSelection
  , tSize
  , tStyle      -- implements D3 transition.style() & D3 transition.styleTween
  , tText
  -- , tween
  ) where

-- || NB. These next functions are not implemented as separate functions - they're rolled up into tAttr and tStyle
  -- , attr
  -- , attrTween    -- rolled up (together with attr, style and styleTween) into tAttr & tStyle
  -- , style
  -- , styleTween

import D3.Base (D3Element, D3, Eff, Filter(..), PolyValue(..))
import D3.Interpolator (Time, D3TweenFn, D3TweenTarget, Index)
import D3.Selection (Selection)
import Data.Function.Eff (mkEffFn2, mkEffFn3, EffFn3, EffFn2, EffFn1, runEffFn3, runEffFn2, runEffFn1)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe, Nullable)
import Prelude (($), (<$>))

foreign import data Transition :: * -> *

foreign import d3TransitionFn    :: ∀ d eff.   EffFn1 (d3::D3|eff) String                                     (Transition d)
foreign import d3TransitionFn2   :: ∀ d eff.                                                 Eff (d3::D3|eff) (Transition d)

foreign import attrFn            :: ∀ d v eff. EffFn3 (d3::D3|eff) String v                    (Transition d) (Transition d)
foreign import durationFn        :: ∀ d eff.   EffFn2 (d3::D3|eff) Time                        (Transition d) (Transition d)
foreign import emptyFn           :: ∀ d eff.   EffFn1 (d3::D3|eff)                             (Transition d) Boolean
foreign import mergeFn           :: ∀ d eff.   EffFn2 (d3::D3|eff) (Transition d)              (Transition d) (Transition d)
foreign import namedTransitionFn :: ∀ d eff.   EffFn2 (d3::D3|eff) String                      (Selection d)  (Transition d)
foreign import nodeFn            :: ∀ d eff.   EffFn1 (d3::D3|eff)                             (Transition d) (Nullable D3Element)
foreign import nodesFn           :: ∀ d eff.   EffFn1 (d3::D3|eff)                             (Transition d) (Array D3Element)
foreign import styleFn           :: ∀ d v eff. EffFn3 (d3::D3|eff) String v                    (Transition d) (Transition d)
foreign import filterFn          :: ∀ d eff.   EffFn2 (d3::D3|eff) String                      (Transition d) (Transition d)
foreign import filterFnP         :: ∀ d eff.   EffFn2 (d3::D3|eff) (d -> Boolean)              (Transition d) (Transition d)
foreign import transitionFn      :: ∀ d eff.   EffFn1 (d3::D3|eff)                             (Selection d)  (Transition d)
foreign import transition2Fn     :: ∀ d eff.   EffFn1 (d3::D3|eff)                             (Transition d) (Transition d)
foreign import delayFn           :: ∀ d eff.   EffFn2 (d3::D3|eff) Time                        (Transition d) (Transition d)
foreign import removeFn          :: ∀ d eff.   EffFn1 (d3::D3|eff)                             (Transition d) (Transition d)
foreign import selectAllFn       :: ∀ d eff.   EffFn2 (d3::D3|eff) String                      (Transition d) (Transition d)
foreign import selectFn          :: ∀ d eff.   EffFn2 (d3::D3|eff) String                      (Transition d) (Transition d)
foreign import selectionFn       :: ∀ d eff.   EffFn1 (d3::D3|eff)                             (Transition d) (Selection d)
foreign import sizeFn            :: ∀ d eff.   EffFn1 (d3::D3|eff)                             (Transition d) Int
foreign import textFn            :: ∀ d v eff. EffFn2 (d3::D3|eff) v                           (Transition d) (Transition d)

-- need to define types to clean these sigs up   TODO
foreign import delayIFn          :: ∀ d eff.   EffFn2 (d3::D3|eff)           (EffFn2 (d3::D3|eff) d Index Time)        (Transition d) (Transition d)
foreign import attrIFn           :: ∀ d v eff. EffFn3 (d3::D3|eff)    String (EffFn3 (d3::D3|eff) d Index D3Element v) (Transition d) (Transition d)
foreign import styleIFn          :: ∀ d v eff. EffFn3 (d3::D3|eff)    String (EffFn3 (d3::D3|eff) d Index D3Element v) (Transition d) (Transition d)

foreign import styleTweenFn      :: ∀ d v eff. EffFn3 (d3::D3|eff)    String (EffFn3 (d3::D3|eff) d Index D3Element v) (Transition d) (Transition d)
foreign import textFnFn          :: ∀ d v v2 eff. EffFn2 (d3::D3|eff)        (EffFn2 (d3::D3|eff) v Index v2) (Transition d) (Transition d)

-- NB when using a saved transition you're going to be using it with different types of selections to this function will
-- morph it from type x to type d, the type of the selection to which it is being applied
foreign import savedTransitionFn :: ∀ d x eff. EffFn2 (d3::D3|eff) (Transition x)              (Selection d)  (Transition d)

type D3DelayFn        d = ∀ eff. d -> Index -> Eff (d3::D3|eff) Time

data DelayValue d = MilliSec Time
                  | DelayFn (D3DelayFn d)

data AttrInterpolator d v =
      Target v  -- straightforward target final value to tween to using built-in interpolators
    | TweenTarget (D3TweenTarget v d) -- function which is called a single time to get a target final value
    | TweenFn     (D3TweenFn     v d) -- function which is called once to generate a function which is then
                                      -- called every tween frame to generate a value

data TransitionName = Name String
                      | Unnamed

d3Transition :: ∀ d eff. TransitionName                             -> Eff (d3::D3|eff) (Transition d)
d3Transition (Name name)    = runEffFn1 d3TransitionFn name
d3Transition Unnamed        = d3TransitionFn2

addTransition :: ∀ d eff.                             Transition d  -> Eff (d3::D3|eff) (Transition d)
addTransition               = runEffFn1 transition2Fn

makeSelection :: ∀ d eff.                             Transition d  -> Eff (d3::D3|eff) (Selection d)
makeSelection              = runEffFn1 selectionFn

makeTransition :: ∀ d eff.                             Selection d  -> Eff (d3::D3|eff) (Transition d)
makeTransition              = runEffFn1 transitionFn

namedTransition :: ∀ d eff. String                  -> Selection d  -> Eff (d3::D3|eff) (Transition d)
namedTransition name        = runEffFn2 namedTransitionFn name

savedTransition :: ∀ d x eff. (Transition x)        -> Selection d  -> Eff (d3::D3|eff) (Transition d)
savedTransition name        = runEffFn2 savedTransitionFn name

duration :: ∀ d eff. Time                           -> Transition d -> Eff (d3::D3|eff) (Transition d)
duration t                  = runEffFn2 durationFn t

delay :: ∀ d eff. DelayValue d                      -> Transition d -> Eff (d3::D3|eff) (Transition d)
delay (MilliSec t)          = runEffFn2 delayFn t
delay (DelayFn f)           = runEffFn2 delayIFn (mkEffFn2 f)

tEmpty :: ∀ d eff.                                     Transition d -> Eff (d3::D3|eff) Boolean
tEmpty                      = runEffFn1 emptyFn

tFilter  :: ∀ d eff.  Filter d                      -> Transition d -> Eff (d3::D3|eff) (Transition d)
tFilter (Selector s)       = runEffFn2 filterFn s
tFilter (Predicate p)      = runEffFn2 filterFnP p

tNode :: ∀ d eff.                                      Transition d -> Eff (d3::D3|eff) (Maybe D3Element)
tNode t                     = toMaybe <$> runEffFn1 nodeFn t

tNodes :: ∀ d eff.                                     Transition d -> Eff (d3::D3|eff) (Array D3Element)
tNodes                      = runEffFn1 nodesFn

tMerge :: ∀ d eff.    Transition d                  -> Transition d -> Eff (d3::D3|eff) (Transition d)
tMerge                      = runEffFn2 mergeFn

tRemove :: ∀ d eff.                                    Transition d -> Eff (d3::D3|eff) (Transition d) -- maybe this should be Void? TODO
tRemove                     = runEffFn1 removeFn

tSize :: ∀ d eff.                                      Transition d -> Eff (d3::D3|eff) Int
tSize                       = runEffFn1 sizeFn

tSelectAll :: ∀ d eff. String                        -> Transition d -> Eff (d3::D3|eff) (Transition d)
tSelectAll selector           = runEffFn2 selectAllFn selector

tSelect  :: ∀ d eff.  String                         -> Transition d -> Eff (d3::D3|eff) (Transition d)
tSelect selector              = runEffFn2 selectFn selector

tText  :: ∀ d v eff.  PolyValue d v                  -> Transition d -> Eff (d3::D3|eff) (Transition d)
tText       (Value value)     = runEffFn2 textFn value
tText       (SetByIndex f)    = runEffFn2 textFnFn (mkEffFn2 f)


tAttr :: ∀ d v eff. String -> AttrInterpolator d v  -> Transition d -> Eff (d3::D3|eff) (Transition d)
tAttr name (Target v)       = runEffFn3 attrFn       name v
tAttr name (TweenTarget f)  = runEffFn3 attrIFn      name (mkEffFn3 f)
tAttr name (TweenFn f)      = runEffFn3 styleTweenFn name (mkEffFn3 f)

tStyle :: ∀ d v eff. String -> AttrInterpolator d v -> Transition d -> Eff (d3::D3|eff) (Transition d)
tStyle name (Target v)      = runEffFn3 styleFn      name v
tStyle name (TweenTarget f) = runEffFn3 styleIFn     name (mkEffFn3 f)
tStyle name (TweenFn f)     = runEffFn3 styleTweenFn name (mkEffFn3 f)
