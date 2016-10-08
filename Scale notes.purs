module Main where

import D3.Scale (rangeRound, bandwidth)
import Data.Int (hexadecimal, toStringAs)

data ContinuousScale d r = {            -- type parameters for domain and range, respectively
    domain      :: (Array d -> scale d r      -> scale d r)  -- sets the domain
  , range       :: (Array r -> scale d r      -> scale d r)  -- sets the range (not necessarily numeric)
  , rangeRound  :: (Array r -> scale d r      -> scale d r)  -- sets the range and sets rounding
  , clamp       :: (Boolean -> scale d r      -> scale d r)  -- limit return to range (domain in case of invert)
  , interpolate :: (Interpolator -> scale d r -> scale d r)  -- specifies interpolator to use
  , nice        :: (Maybe Number -> scale d r -> scale d r) --
  , scale       :: (d -> scale d r            -> r)          -- use the scale to map d to r
  , invert      :: (r -> scale d r            -> d)          -- mapping back from range to domain
  , ticks       :: (Maybe Number -> scale d r -> Array d)  -- get a list of n vals from domain
  , tickFormat  :: (Number -> Maybe Format -> scale d r -> (Number -> String))
}

data LogScale = {
    domain      :: continuousScale.domain
  , range       :: continuousScale.range
  , rangeRound  :: continuousScale.rangeRound
  , clamp       :: continuousScale.clamp
  , interpolate :: continuousScale.interpolate
  , nice        :: -- custom for base
  , invert      :: continuousScale.invert
  , scale       :: continuousScale.scale
  , ticks       :: -- custom for log scale
  , tickFormat  :: -- custom for log scale
  , base        :: (Number -> logScale d r -> logScale d r) -- set the base for this logScale

}

data PowScale = {
    domain      :: continuousScale.domain
  , range       :: continuousScale.range
  , rangeRound  :: continuousScale.rangeRound
  , clamp       :: continuousScale.clamp
  , interpolate :: continuousScale.interpolate
  , nice        :: continuousScale.nice
  , invert      :: continuousScale.invert
  , scale       :: continuousScale.scale
  , ticks       :: continuousScale.ticks
  , tickFormat  :: continuousScale.tickFormat
}

data TimeScale = {      -- domain is JavaScript Date
    domain      :: continuousScale.domain
  , range       :: continuousScale.range
  , rangeRound  :: continuousScale.rangeRound
  , clamp       :: continuousScale.clamp
  , interpolate :: continuousScale.interpolate
  , nice        :: continuousScale.nice
  , invert      :: continuousScale.invert
  , scale       :: continuousScale.scale
  , ticks       ::  -- custom, returns Dates as the ticks
  , tickFormat  :: continuousScale.tickFormat
}

data IdentityScale = {
    domain      :: continuousScale.domain
  , range       :: continuousScale.range
  , rangeRound  :: continuousScale.rangeRound  -- disabled
  , clamp       :: continuousScale.clamp       -- disabled
  , interpolate :: continuousScale.interpolate -- disabled
  , nice        :: continuousScale.nice
  , invert      :: continuousScale.invert
  , scale       :: continuousScale.scale
  , ticks       :: continuousScale.ticks
  , tickFormat  :: continuousScale.tickFormat
}

data LinearScale = {
    domain      :: continuousScale.domain
  , range       :: continuousScale.range
  , rangeRound  :: continuousScale.rangeRound
  , clamp       :: continuousScale.clamp
  , interpolate :: continuousScale.interpolate
  , nice        :: continuousScale.nice
  , invert      :: continuousScale.invert
  , scale       :: continuousScale.scale
  , ticks       :: continuousScale.ticks
  , tickFormat  :: continuousScale.tickFormat
}


data OrdinalScale d r = {  -- domain doesn't have to be numeric
    domain      :: Array d -> scale d r -> scale d r
    range       :: Array r -> scale d r -> scale d r
    unknown     :: Maybe r -> scale d r -> scale d r -- defaults to implicit extension of domain when unknowns are provided
}

data BandScale = {
    domain      :: ordinalScale.domain
  , range       :: ordinalScale.range
  , unknown     :: ordinalScale.unknown
  , rangeRound  ::
  , padding     ::
  , paddingInner ::
  , paddingOuter ::
  , align        ::
  , bandwidth    ::
  , step         ::
  , align        ::
}

data PointScale = {
    domain      :: ordinalScale.domain
  , range       :: ordinalScale.range
  , unknown     :: ordinalScale.unknown   -- disabled
  , rangeRound  :: bandScale.rangeRound
  , padding     :: bandScale.padding
  , paddingInner :: bandScale.paddingInner
  , paddingOuter :: bandScale.paddingOuter
  , align        :: bandScale.align
  , bandwidth    :: bandScale.bandwidth     -- returns zero
  , step         :: bandScale.step
  , align        :: bandScale.align
}

data CategoryScale = {
    domain      :: ordinalScale.domain
  , range       :: ordinalScale.range
  , unknown     :: ordinalScale.unknown
}

data SequentialScale = {  -- no invert, range, rangeRound, interpolate
    domain      :: continuousScale.domain
  , clamp       :: continuousScale.clamp
  , interpolator :: Interpolator -> scale d r -> scale d r
}

type RGBString :: String  -- constrained to format "#AABBCC"
data RGB :: RGB String String String
makeRGB :: Int -> Int -> Int -> Maybe RGB
makeRGB r g b = if valid r && valid g && valid b
                then Just (RGB (toStringAs hexadecimal r) (toStringAs hexadecimal g) (toStringAs hexadecimal b))
                else Nothing

instance showRGB = Show RGB where
  show (RGB r g b) = "#" <> r <> g <> b

type Interpolator :: (Number -> String)

  d3.interpolateViridis - a dark-to-light color scheme.
  d3.interpolateInferno - a dark-to-light color scheme.
  d3.interpolateMagma - a dark-to-light color scheme.
  d3.interpolatePlasma - a dark-to-light color scheme.
  d3.interpolateWarm - a rotating-hue color scheme.
  d3.interpolateCool - a rotating-hue color scheme.
  d3.interpolateRainbow - a cyclical rotating-hue color scheme.
  d3.interpolateCubehelixDefault - a dark-to-light, rotating-hue color scheme.


data QuantizeScale {
    domain :: [Coercable1, Coercable2] -> scale d r -> scale d r
  , range  :: [Things] -> scale d r -> scale d r
  , nice        :: continuousScale.nice
  , ticks       :: continuousScale.ticks
  , tickFormat  :: continuousScale.tickFormat
  , invertExtent :: r -> Array d -> scale d r  -- Returns the extent of values in the domain for the corresponding value in the range
}

data QuantileScale {
    domain ::
  , range  ::
  , invertExtent ::
  , quantiles ::  -- get the quantile thresholds.
}

data ThresholdScale {
    domain :: -- ascending array of Ord
    range ::
    invertExtent ::
}
