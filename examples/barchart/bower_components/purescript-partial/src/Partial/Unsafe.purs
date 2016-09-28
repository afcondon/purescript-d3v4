-- | Utilities for working with partial functions.
module Partial.Unsafe
  ( unsafePartial
  , unsafeCrashWith
  ) where

import Partial (crashWith)

-- | Discharge a partiality constraint, unsafely.
foreign import unsafePartial :: forall a. (Partial => a) -> a

-- | A function which crashes with the specified error message.
unsafeCrashWith :: forall a. String -> a
unsafeCrashWith msg = unsafePartial (crashWith msg)
