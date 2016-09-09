module D3.Interpolator
  (Interpolator, Time) where

import D3.Base (D3Element)

type Time = Number

-- foreign import data InterpolatorFn :: * -> *

type Interpolator v d = (d -> Number -> D3Element -> (Time -> v))
