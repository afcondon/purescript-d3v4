module D3.Scale
  ( Scale
  , ScaleType(..)
  , SchemeCategory
  , schemeCategory10
  , schemeCategory20
  , schemeCategory20b
  , schemeCategory20c
  , d3Scale
  , bandwidth
  , clamp
  , domain
  , interpolate
  , invert
  , nice
  , padding
  , paddingInner
  , paddingOuter
  , range
  , rangeRound
  , round
  , scaleBy
  , tickFormat
  , ticks
  ) where

import Control.Monad.Eff (Eff)
import D3.Base (D3)
import D3.Collections
import Data.Function.Eff (runEffFn2, runEffFn3, runEffFn1, EffFn2, EffFn3, EffFn1)
import Data.Maybe (Maybe(..))

foreign import data Scale :: * -> * -> *

type Interpolator = (Number -> String)
type Format       = String

-- | This belongs in d3-scale-chromatic here just for now, to be broken out later
foreign import data SchemeCategory :: *       -- in practice it's just a string, i think
foreign import schemeCategory10  :: SchemeCategory
foreign import schemeCategory20  :: SchemeCategory
foreign import schemeCategory20b :: SchemeCategory
foreign import schemeCategory20c :: SchemeCategory

type D3LinearScale d r    = Scale d r
type D3LogScale d r       = Scale d r
type D3PowerScale d r     = Scale d r
type D3IdentityScale d r  = Scale d r
type D3TimeScale d r      = Scale d r
type D3BandScale d r      = Scale d r
type D3PointScale d r     = Scale d r
type D3CategoryScale d r  = Scale d r
type D3QuantizeScale d r  = Scale d r
type D3QuantileScale d r  = Scale d r
type D3ThresholdScale d r = Scale d r

-- || The Continuous Scale Constructors
foreign import d3LinearScaleFn   :: ∀ d r eff. Eff (d3::D3|eff) (D3LinearScale d r)
foreign import d3LogScaleFn      :: ∀ d r eff. Eff (d3::D3|eff) (D3LogScale d r)
foreign import d3PowerScaleFn    :: ∀ d r eff. Eff (d3::D3|eff) (D3PowerScale d r)
foreign import d3IdentityScaleFn :: ∀ d r eff. Eff (d3::D3|eff) (D3IdentityScale d r)
foreign import d3TimeScaleFn     :: ∀ d r eff. Eff (d3::D3|eff) (D3TimeScale d r)

-- || The Ordinal Scale Constructors
foreign import d3BandScaleFn     :: ∀ d r eff. Eff (d3::D3|eff)                   (D3BandScale d r)
foreign import d3PointScaleFn    :: ∀ d r eff. Eff (d3::D3|eff)                   (D3PointScale d r)
foreign import d3CategoryScaleFn :: ∀ d r eff. EffFn1 (d3::D3|eff) SchemeCategory (D3CategoryScale d r)

-- || Other Scale Constructors
foreign import d3QuantizeScaleFn :: ∀ d r eff. Eff (d3::D3|eff)                   (D3QuantizeScale d r)
foreign import d3QuantileScaleFn :: ∀ d r eff. Eff (d3::D3|eff)                   (D3QuantileScale d r)
foreign import d3ThresholdScaleFn :: ∀ d r eff. Eff (d3::D3|eff)                  (D3ThresholdScale d r)

-- functions
foreign import domainArrFn   :: ∀ d r eff. EffFn2 (d3::D3|eff) (Array d)             (Scale d r) (Scale d r)
foreign import domainMapFn   :: ∀ d r eff. EffFn2 (d3::D3|eff) (D3Map d)             (Scale d r) (Scale d r)
foreign import rangeFn       :: ∀ d r eff. EffFn3 (d3::D3|eff) r r                   (Scale d r) (Scale d r)
foreign import rangeRoundFn  :: ∀ d r eff. EffFn3 (d3::D3|eff) r r                   (Scale d r) (Scale d r)
foreign import roundFn       :: ∀ d r eff. EffFn2 (d3::D3|eff) Boolean               (Scale d r) (Scale d r)
foreign import clampFn       :: ∀ d r eff. EffFn2 (d3::D3|eff) Boolean               (Scale d r) (Scale d r)
foreign import interpolateFn :: ∀ d r eff. EffFn2 (d3::D3|eff) Interpolator          (Scale d r) (Scale d r)
foreign import niceFn        :: ∀ d r eff. EffFn1 (d3::D3|eff)                       (Scale d r) (Scale d r)
foreign import nicePFn       :: ∀ d r eff. EffFn2 (d3::D3|eff) Number                (Scale d r) (Scale d r)
foreign import invertFn      :: ∀ d r eff. EffFn2 (d3::D3|eff) r                     (Scale d r) d
foreign import ticksFn       :: ∀ d r eff. EffFn1 (d3::D3|eff)                       (Scale d r) (Array d)
foreign import ticksPFn      :: ∀ d r eff. EffFn2 (d3::D3|eff) Number                (Scale d r) (Array d)
foreign import tickFormatFn  :: ∀ d r eff. EffFn2 (d3::D3|eff) Number                (Scale d r) (Number -> String)
foreign import tickFormatPFn :: ∀ d r eff. EffFn3 (d3::D3|eff) Number Format         (Scale d r) (Number -> String)
foreign import applyScaleFn  :: ∀ d r v eff. EffFn2 (d3::D3|eff) (Scale d r)         v            Number

-- || Foreign functions for the BandScale type
foreign import paddingFn         :: ∀ d r eff. EffFn2 (d3::D3|eff) Number (D3BandScale d r) (D3BandScale d r)
foreign import paddingInnerFn    :: ∀ d r eff. EffFn2 (d3::D3|eff) Number (D3BandScale d r) (D3BandScale d r)
foreign import paddingOuterFn    :: ∀ d r eff. EffFn2 (d3::D3|eff) Number (D3BandScale d r) (D3BandScale d r)
foreign import bandwidthFn       :: ∀ d r eff. EffFn1 (d3::D3|eff)        (D3BandScale d r) Number

-- | Put at least a bit of structure on the construction of the Scales even tho they end up being untyped after creation
data ScaleType = Band
               | Category SchemeCategory
               | Identity
               | Linear
               | Log
               | Point
               | Power
               | Quantile
               | Quantize
               | Threshold
               | Time

d3Scale :: ∀ d r eff. ScaleType  -> Eff (d3::D3|eff) (Scale d r)
d3Scale (Category scheme) = runEffFn1 d3CategoryScaleFn scheme
d3Scale Band       = d3BandScaleFn
d3Scale Identity   = d3IdentityScaleFn
d3Scale Linear     = d3LinearScaleFn
d3Scale Log        = d3LogScaleFn
d3Scale Point      = d3PointScaleFn
d3Scale Power      = d3PowerScaleFn
d3Scale Quantile   = d3QuantileScaleFn
d3Scale Quantize   = d3QuantizeScaleFn
d3Scale Threshold  = d3ThresholdScaleFn
d3Scale Time       = d3TimeScaleFn

-- || Scale functions, not all available to all Scales, caution! TODO
scaleBy :: ∀ d r v eff. Scale d r -> v -> Eff (d3::D3|eff) Number
scaleBy s = runEffFn2 applyScaleFn s

-- sets the domain
domain :: ∀ d r eff. (D3Collection d) -> Scale d r         -> Eff (d3::D3|eff)(Scale d r)
domain (D3ArrT array)      = runEffFn2 domainArrFn array
domain (D3MapT map)        = runEffFn2 domainMapFn map
domain (D3Range start end) = runEffFn2 domainArrFn [start, end]

-- sets the range (not necessarily numeric)
range :: ∀ d r eff. r -> r  -> Scale d r          -> Eff (d3::D3|eff)(Scale d r)
range = runEffFn3 rangeFn

-- sets the range and sets rounding
rangeRound :: ∀ d r eff. r -> r  -> Scale d r     -> Eff (d3::D3|eff)(Scale d r)
rangeRound = runEffFn3 rangeRoundFn

-- set rounding on or off
round :: ∀ d r eff. Boolean -> Scale d r           -> Eff (d3::D3|eff)(Scale d r)
round = runEffFn2 roundFn

-- limit return to range (domain in case of invert)
clamp :: ∀ d r eff. Boolean -> Scale d r           ->Eff (d3::D3|eff) (Scale d r)
clamp = runEffFn2 clampFn

-- specifies interpolator to use
interpolate :: ∀ d r eff. Interpolator -> Scale d r     ->Eff (d3::D3|eff) (Scale d r)
interpolate = runEffFn2 interpolateFn

nice :: ∀ d r eff. (Maybe Number) -> Scale d r          -> Eff (d3::D3|eff) (Scale d r)
nice (Just count) = runEffFn2 nicePFn count
nice Nothing      = runEffFn1 niceFn

-- maps back from range to domain, NB invert can't be called on SequentialScales
invert :: ∀ d r eff. r -> (Scale d r)                ->  Eff (d3::D3|eff) d
invert = runEffFn2 invertFn

-- get a list of n vals from domain
ticks :: ∀ d r eff. (Maybe Number) -> Scale d r       -> Eff (d3::D3|eff) (Array d)
ticks (Just n) = runEffFn2 ticksPFn n
ticks Nothing  = runEffFn1 ticksFn

tickFormat :: ∀ d r eff. Number -> Maybe Format -> Scale d r   -> Eff (d3::D3|eff) (Number -> String)
tickFormat n (Just f) = runEffFn3 tickFormatPFn n f
tickFormat n Nothing  = runEffFn2 tickFormatFn n


-- these functions should only apply to scales of type band, tricky to express the
-- way it's written right now...TODO revisit the types here
padding      :: ∀ d r eff. Number -> D3BandScale d r -> Eff (d3::D3|eff) (D3BandScale d r)
padding      = runEffFn2 paddingFn

paddingInner :: ∀ d r eff. Number -> D3BandScale d r -> Eff (d3::D3|eff) (D3BandScale d r)
paddingInner = runEffFn2 paddingInnerFn

paddingOuter :: ∀ d r eff. Number -> D3BandScale d r -> Eff (d3::D3|eff) (D3BandScale d r)
paddingOuter = runEffFn2 paddingOuterFn

bandwidth    :: ∀ d r eff.           D3BandScale d r -> Eff (d3::D3|eff) Number
bandwidth    = runEffFn1 bandwidthFn
