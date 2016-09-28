module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.DateTime as DT
import Data.Enum (toEnum)
import Data.Either (isRight)
import Data.Foreign (F, Foreign)
import Data.Foreign.Class (read)
import Data.JSDate as JSD
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)

import Global (nan)

import Test.Assert (ASSERT, assert)

foreign import myDate :: Foreign

main :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, locale :: JSD.LOCALE | eff) Unit
main = do

  log "Check IsForeign intance will read JS date values..."
  assert $ isRight $ read myDate :: F JSD.JSDate

  log "Checking that a UTC date constructed with sensible values is valid..."
  assert $ JSD.isValid $ JSD.jsdate defaultDateRecord

  log "Checking that any UTC date constructed with a NaN value is invalid..."
  assert $ not $ JSD.isValid $ JSD.jsdate defaultDateRecord { year = nan }
  assert $ not $ JSD.isValid $ JSD.jsdate defaultDateRecord { month = nan }
  assert $ not $ JSD.isValid $ JSD.jsdate defaultDateRecord { day = nan }
  assert $ not $ JSD.isValid $ JSD.jsdate defaultDateRecord { hour = nan }
  assert $ not $ JSD.isValid $ JSD.jsdate defaultDateRecord { minute = nan }
  assert $ not $ JSD.isValid $ JSD.jsdate defaultDateRecord { second = nan }
  assert $ not $ JSD.isValid $ JSD.jsdate defaultDateRecord { millisecond = nan }

  log "Checking that a date constructed from a string with sensible values is valid..."
  assert <<< JSD.isValid =<< JSD.parse "2011-10-10T14:48:00"
  assert <<< JSD.isValid =<< JSD.parse "Thu, 01 Jan 1970 00:00:00 GMT-0400"

  log "Checking that a local date constructed with sensible values is valid..."
  assert <<< JSD.isValid =<< JSD.jsdateLocal defaultDateRecord

  log "Checking that any local date constructed with a NaN value is invalid..."
  assert <<< not <<< JSD.isValid =<< JSD.jsdateLocal defaultDateRecord { year = nan }
  assert <<< not <<< JSD.isValid =<< JSD.jsdateLocal defaultDateRecord { month = nan }
  assert <<< not <<< JSD.isValid =<< JSD.jsdateLocal defaultDateRecord { day = nan }
  assert <<< not <<< JSD.isValid =<< JSD.jsdateLocal defaultDateRecord { hour = nan }
  assert <<< not <<< JSD.isValid =<< JSD.jsdateLocal defaultDateRecord { minute = nan }
  assert <<< not <<< JSD.isValid =<< JSD.jsdateLocal defaultDateRecord { second = nan }
  assert <<< not <<< JSD.isValid =<< JSD.jsdateLocal defaultDateRecord { millisecond = nan }

  log "Check that a roundtrip conversion of a dates results in the input"
  assert $ JSD.toDateTime (JSD.fromDateTime dateTime) == Just dateTime
  assert $ JSD.toDateTime (JSD.fromDateTime bottom) == Just bottom
  assert $ JSD.toDateTime (JSD.fromDateTime top) == Just top

  log "All tests done"

  where

  defaultDateRecord =
    { year: 2016.0
    , month: 5.0
    , day: 3.0
    , hour: 2.0
    , minute: 21.0
    , second: 43.0
    , millisecond: 678.0
    }

  date = unsafePartial $ fromJust $
    DT.canonicalDate <$> toEnum 2016 <*> pure DT.June <*> toEnum 3

  time = unsafePartial $ fromJust $
    DT.Time <$> toEnum 2 <*> toEnum 21 <*> toEnum 43 <*> toEnum 678

  dateTime = DT.DateTime date time
