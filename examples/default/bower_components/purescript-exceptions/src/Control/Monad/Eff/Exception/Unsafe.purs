module Control.Monad.Eff.Exception.Unsafe where

import Control.Monad.Eff.Exception (Error, error, throwException)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Semigroupoid ((<<<))

-- | Throw an exception in pure code. This function should be used very
-- | sparingly, as it can cause unexpected crashes at runtime.
unsafeThrowException :: forall a. Error -> a
unsafeThrowException = unsafePerformEff <<< throwException

-- | Defined as `unsafeThrowException <<< error`.
unsafeThrow :: forall a. String -> a
unsafeThrow = unsafeThrowException <<< error
