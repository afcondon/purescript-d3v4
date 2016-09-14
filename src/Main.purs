module Main where

import D3.Selection
import Control.Monad.Eff.Console (CONSOLE, log)
import D3.Base (D3, Eff, DataBind(Data), PolyValue(SetByIndex), (..))


import DOM.HTML.Event.EventTypes (mouseenter)
import Data.Foldable (foldr)
import Prelude (Unit, unit, pure, show, bind, max, ($), (*), (<>))

-- | mainline: simplest possible D3 demo
array :: Array Number
array = [4.0, 8.0, 15.0, 16.0, 23.0, 42.0]

arrayMax :: Number
arrayMax = foldr max 0.0 array

awn :: forall eff. CallbackParam Number -> Eff (d3::D3, console::CONSOLE|eff) Unit
awn { datum: d, meta: m } =
  do
    log (show d)
    log (show m)
    pure unit

main :: forall e. Eff (d3::D3,console::CONSOLE|e) Unit
main = do
  -- | a simple chart made of `div`s from a data array of Numbers
  chartN <- d3Select ".chart"
      .. selectAll "div"
        .. dataBind (Data array)
      .. enter .. append "div"
        .. style    "width"         (SetByIndex (\d i -> pure $ show (d * 10.0) <> "px"))
        .. text                     (SetByIndex (\d i -> pure $ show d))
        .. on       mouseenter      awn

  pure unit
