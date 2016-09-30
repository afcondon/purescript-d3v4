module Main where


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

data logScale = {
    domain      :: continuousScale.domain
  , range       :: continuousScale.range
  , rangeRound  :: continuousScale.rangeRound
  , clamp       :: continuousScale.clamp
  , interpolate :: continuousScale.interpolate
  , nice        ::
  , invert      :: continuousScale.invert
  , scale       :: continuousScale.scale
  , ticks       ::
  , tickFormat  ::
  , base        :: (Number -> logScale d r -> logScale d r) -- set the base for this logScale

}

data powScale = {
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

data timeScale = {      -- domain is JavaScript Date
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

data identityScale = {
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

data linearScale = {
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
