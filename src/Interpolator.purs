module D3.Interpolator
  ( Time
  , Index
  , D3TweenFnUncurried
  , D3TweenTargetUncurried
  , D3TweenFn
  , D3TweenTarget
  , D3EffTweenFn
  , D3EffTweenTarget
  -- , mkTweenTargetEffFn
  -- , mkTweenFunctionEffFn
  ) where

import D3.Base (D3, Eff, D3Element)
type Time  = Number
type Index = Number

-- foreign import data TweenFn :: * -> *
foreign import data D3EffTweenTarget ::     # ! -> * -> * -> * -> *                          -> *
type D3TweenTargetUncurried v d = ∀ eff.
                  D3EffTweenTarget (d3::D3|eff)    d    Index     D3Element                     v
type D3TweenTarget          v d = ∀ eff.
                                                  (d -> Index ->  D3Element -> Eff (d3::D3|eff) v)

foreign import data D3EffTweenFn ::     # ! -> * -> * -> * -> *                           -> *
type D3TweenFnUncurried v d = ∀ eff.
                  D3EffTweenFn (d3::D3|eff)    d    Index     D3Element                     (Time -> v)
type D3TweenFn          v d = ∀ eff.
                                              (d -> Index -> D3Element -> Eff (d3::D3|eff) (Time -> v))
