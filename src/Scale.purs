module D3.Scale
  ( d3ContinuousScale
  , d3OrdinalScale
  , ContinuousScaleType (..)
  , ScaleContinuous
  , OrdinalScaleType (..)
  , ScaleOrdinal
  , SchemeCategory
  , rangeRound
  , class Ranged
  , padding
  , paddingInner
  , paddingOuter
  , bandwidth
  , class Scale
  , scale
  ) where

import Control.Monad.Eff (Eff)
import D3.Base (D3)
import Data.Function.Eff (EffFn2, runEffFn2, EffFn3, EffFn1, runEffFn3, runEffFn1)

foreign import data ScaleContinuous :: * -> * -> *
foreign import data ScaleOrdinal    :: * -> * -> *
foreign import data ScaleSequential :: * -> * -> *
foreign import data ScaleQuantize   :: * -> * -> *
foreign import data ScaleQuantile   :: * -> * -> *
foreign import data ScaleThreshold  :: * -> * -> *

foreign import d3LinearScaleFn   :: ∀ d r eff. Eff (d3::D3|eff) (ScaleContinuous d r)
foreign import d3LogScaleFn      :: ∀ d r eff. Eff (d3::D3|eff) (ScaleContinuous d r)
foreign import d3PowerScaleFn    :: ∀ d r eff. Eff (d3::D3|eff) (ScaleContinuous d r)
foreign import d3IdentityScaleFn :: ∀ d r eff. Eff (d3::D3|eff) (ScaleContinuous d r)
foreign import d3TimeScaleFn     :: ∀ d r eff. Eff (d3::D3|eff) (ScaleContinuous d r)

foreign import d3BandScaleFn     :: ∀ d r eff. Eff (d3::D3|eff)                   (ScaleOrdinal d r)
foreign import d3PointScaleFn    :: ∀ d r eff. Eff (d3::D3|eff)                   (ScaleOrdinal d r)
foreign import d3CategoryScaleFn :: ∀ d r eff. EffFn1 (d3::D3|eff) SchemeCategory (ScaleOrdinal d r)

foreign import paddingFn         :: ∀ d r eff. EffFn2 (d3::D3|eff) Number (ScaleOrdinal d r) (ScaleOrdinal d r)
foreign import paddingInnerFn    :: ∀ d r eff. EffFn2 (d3::D3|eff) Number (ScaleOrdinal d r) (ScaleOrdinal d r)
foreign import paddingOuterFn    :: ∀ d r eff. EffFn2 (d3::D3|eff) Number (ScaleOrdinal d r) (ScaleOrdinal d r)
foreign import bandwidthFn       :: ∀ d r eff. EffFn1 (d3::D3|eff)        (ScaleOrdinal d r) Number

-- | This belongs in d3-scale-chromatic here just for now, to be broken out later
foreign import data SchemeCategory :: *       -- in practice it's just a string, i think

data ContinuousScaleType = Linear | Log | Power | Identity | Time
data OrdinalScaleType    = Band   | Point |  Category SchemeCategory
data SequentialScaleType = Sequential
data QuantizeScaleType   = Quantize
data QuantileScaleType   = Quantile
data ThresholdScaleType  = Threshold


d3ContinuousScale :: ∀ d r eff. ContinuousScaleType  -> Eff (d3::D3|eff) (ScaleContinuous d r)
d3ContinuousScale Linear   = d3LinearScaleFn
d3ContinuousScale Log      = d3LogScaleFn
d3ContinuousScale Power    = d3PowerScaleFn
d3ContinuousScale Identity = d3IdentityScaleFn
d3ContinuousScale Time     = d3TimeScaleFn

d3OrdinalScale :: ∀ d r eff. OrdinalScaleType       -> Eff (d3::D3|eff) (ScaleOrdinal d r)
d3OrdinalScale Band              = d3BandScaleFn
d3OrdinalScale Point             = d3PointScaleFn
d3OrdinalScale (Category scheme) = runEffFn1 d3CategoryScaleFn scheme

-- | rangeRounds can be applied to various scale types, using a typeclass to capture this
-- | Probably want to use the type system to distinguish between rangeRound[start, end] and rangeRound[v1, v2, v3...]
foreign import rangeRoundFn :: ∀ s eff.     EffFn3 (d3::D3|eff) Number Number s s
foreign import applyScaleFn :: ∀ s d r eff. EffFn2 (d3::D3|eff) d s r

rangeRoundConstrained :: ∀ s eff.     (Ranged s) => Number -> Number -> s -> Eff (d3::D3|eff) s
rangeRoundConstrained start end ranged = runEffFn3 rangeRoundFn start end ranged

applyScaleConstrained :: ∀ s d r eff. (Scale s)  => d      -> s -> Eff (d3::D3|eff) r
applyScaleConstrained d scale          = runEffFn2 applyScaleFn d scale

class Ranged a where
  rangeRound :: ∀ eff. Number -> Number -> a -> Eff (d3::D3|eff) a

instance rangeRoundContinuous :: Ranged (ScaleContinuous d r) where
  rangeRound start end = rangeRoundConstrained start end

instance rangeRoundOrdinal :: Ranged (ScaleOrdinal d r) where
  rangeRound start end = rangeRoundConstrained start end

-- | all the various scale types can be applied as a function, using a typeclass to capture this
class Scale a where
  scale :: ∀ d r eff. d -> a -> Eff (d3::D3|eff) r

instance scaleContinuous :: Scale (ScaleContinuous d r) where
  scale d = applyScaleConstrained d

instance scaleOrdinal :: Scale (ScaleOrdinal d r) where
  scale d = applyScaleConstrained d

-- these functions should only apply to scales of type band, tricky to express the
-- way it's written right now...TODO revisit the types here

padding      :: ∀ d r eff. Number -> ScaleOrdinal d r -> Eff (d3::D3|eff) (ScaleOrdinal d r)
padding      = runEffFn2 paddingFn

paddingInner :: ∀ d r eff. Number -> ScaleOrdinal d r -> Eff (d3::D3|eff) (ScaleOrdinal d r)
paddingInner = runEffFn2 paddingInnerFn

paddingOuter :: ∀ d r eff. Number -> ScaleOrdinal d r -> Eff (d3::D3|eff) (ScaleOrdinal d r)
paddingOuter = runEffFn2 paddingOuterFn

bandwidth    :: ∀ d r eff.           ScaleOrdinal d r -> Eff (d3::D3|eff) Number
bandwidth    = runEffFn1 bandwidthFn
