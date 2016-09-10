module D3.Interpolator
  ( Time
  , D3TweenFnUncurried
  , D3TweenTargetUncurried
  , D3TweenFn
  , D3TweenTarget
  , D3EffTweenFn
  , D3EffTweenTarget
  , mkTweenTargetEffFn
  , mkTweenFunctionEffFn
  ) where

import D3.Base (D3Element)

type Time = Number

-- foreign import data TweenFn :: * -> *
foreign import data D3EffTweenTarget ::            # ! -> * ->   * -> *   ->    *         -> *
type D3TweenTargetUncurried v d = ∀ eff. D3EffTweenTarget eff    d    Number    D3Element    v
type D3TweenTarget          v d =                               (d -> Number -> D3Element -> v)

foreign import data D3EffTweenFn ::        # ! -> * ->   * -> *   ->    *         -> *
type D3TweenFnUncurried v d = ∀ eff. D3EffTweenFn eff    d    Number    D3Element    (Time -> v)
type D3TweenFn          v d =                           (d -> Number -> D3Element -> (Time -> v))

-- foreign functions (actually just one function under the hood)
foreign import mkTweenTargetEffFn   :: ∀ v d. D3TweenTarget d v -> D3TweenTargetUncurried d v
foreign import mkTweenFunctionEffFn :: ∀ v d. D3TweenFn     d v -> D3TweenFnUncurried     d v
