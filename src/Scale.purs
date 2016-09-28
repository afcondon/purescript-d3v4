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
  ) where

import Control.Monad.Eff (Eff)
import D3.Base (D3)
import Data.Function.Eff (EffFn3, EffFn1, runEffFn3, runEffFn1)

foreign import data ScaleContinuous :: *
foreign import data ScaleOrdinal    :: *
foreign import data ScaleSequential :: *
foreign import data ScaleQuantize   :: *
foreign import data ScaleQuantile   :: *
foreign import data ScaleThreshold  :: *

foreign import d3LinearScaleFn   :: ∀ eff. Eff (d3::D3|eff) ScaleContinuous
foreign import d3LogScaleFn      :: ∀ eff. Eff (d3::D3|eff) ScaleContinuous
foreign import d3PowerScaleFn    :: ∀ eff. Eff (d3::D3|eff) ScaleContinuous
foreign import d3IdentityScaleFn :: ∀ eff. Eff (d3::D3|eff) ScaleContinuous
foreign import d3TimeScaleFn     :: ∀ eff. Eff (d3::D3|eff) ScaleContinuous

foreign import d3BandScaleFn     :: ∀ eff. Eff (d3::D3|eff)                   ScaleOrdinal
foreign import d3PointScaleFn    :: ∀ eff. Eff (d3::D3|eff)                   ScaleOrdinal
foreign import d3CategoryScaleFn :: ∀ eff. EffFn1 (d3::D3|eff) SchemeCategory ScaleOrdinal

-- | Incorporating some of d3-scale-chromatic here just for now, to be broken out later

foreign import data SchemeCategory :: *       -- in practice it's just a string, i think

data ContinuousScaleType = Linear | Log | Power | Identity | Time
data OrdinalScaleType    = Band   | Point |  Category SchemeCategory
data SequentialScaleType = Sequential
data QuantizeScaleType   = Quantize
data QuantileScaleType   = Quantile
data ThresholdScaleType  = Threshold

-- | Probably want to use the type system to distinguish between rangeRound[start, end] and rangeRound[v1, v2, v3...]
foreign import rangeRoundCFn :: ∀ eff. EffFn3 (d3::D3|eff) Number Number ScaleContinuous ScaleContinuous
foreign import rangeRoundOFn :: ∀ eff. EffFn3 (d3::D3|eff) Number Number ScaleOrdinal    ScaleOrdinal


d3ContinuousScale :: ∀ eff. ContinuousScaleType -> Eff (d3::D3|eff) ScaleContinuous
d3ContinuousScale Linear   = d3LinearScaleFn
d3ContinuousScale Log      = d3LogScaleFn
d3ContinuousScale Power    = d3PowerScaleFn
d3ContinuousScale Identity = d3IdentityScaleFn
d3ContinuousScale Time     = d3TimeScaleFn

d3OrdinalScale :: ∀ eff. OrdinalScaleType       -> Eff (d3::D3|eff) ScaleOrdinal
d3OrdinalScale Band              = d3BandScaleFn
d3OrdinalScale Point             = d3PointScaleFn
d3OrdinalScale (Category scheme) = runEffFn1 d3CategoryScaleFn scheme

class Ranged a where
  rangeRound :: ∀ eff. Number -> Number -> a -> Eff (d3::D3|eff) a

instance rangeRoundContinuous :: Ranged ScaleContinuous where
  rangeRound start end = runEffFn3 rangeRoundCFn start end

instance rangeRoundOrdinal :: Ranged ScaleOrdinal where
  rangeRound start end = runEffFn3 rangeRoundOFn start end
