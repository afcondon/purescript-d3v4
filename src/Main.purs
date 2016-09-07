module Main where

import Prelude (Unit, show, unit, pure, bind, max, (*), (<>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM.HTML.Event.EventTypes (mouseenter, mouseleave, click)
import Data.Foldable (foldr)
import Data.String (length)

import D3.Selection

-- | mainline: simplest possible D3 demo
array :: Array Number
array = [4.0, 8.0, 15.0, 16.0, 23.0, 42.0]

array2 :: Array String
array2 = ["this", "is", "a", "sentence", "in", "pieces"]

arrayMax :: Number
arrayMax = foldr max 0.0 array

awn :: forall eff. CallbackParam Number -> Eff (d3::D3, console::CONSOLE|eff) Unit
awn { datum: d, meta: m } = do
  log (show d)
  log (show m)
  pure unit

bel :: forall eff. CallbackParamP Number String -> Eff (d3::D3, console::CONSOLE|eff) Unit
bel { datum: d, prop: p } = do
  log (show d)
  log (show p)
  pure unit

main :: forall e. Eff ( d3 :: D3 , console :: CONSOLE | e ) Unit
main = do
  rootSelect ".chart"
    .. selectAll "div"
      .. bindData array
    .. enter .. append "div"
      .. style "width" (FnD (\d -> show (d * 10.0) <> "px"))
      .. text          (FnD (\d -> show d))
      .. on mouseenter         awn
      .. on mouseleave         awn
      .. on' click "magic" "snape" bel
  rootSelect ".chart2"
    .. selectAll "div"
      .. bindData array2
    .. enter .. append "div"
      .. style "background-color"  (Value "red")
      .. style "width" (FnD (\d -> show ((length d) * 20) <> "px"))
      .. text          (FnD (\d -> show d))
      .. on' click "cep" "stringy" bel
  pure unit
