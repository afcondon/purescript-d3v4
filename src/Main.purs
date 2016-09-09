module Main where

import D3.Selection
import Control.Monad.Eff.Console (CONSOLE, log)
import D3.Base (D3, Eff, D3Element, Nodes, Index, theHorror, (..), (...))
import D3.Transitions (Transition, AttrInterpolator(..), duration, makeTransition, namedTransition, savedTransition, d3Transition, tStyle, tAttr)
import DOM.HTML.Event.EventTypes (mouseenter, mouseleave, click)
import Data.Array (reverse)
import Data.Foldable (foldr)
import Data.String (length, toCharArray, fromCharArray)
import Prelude (Unit, show, unit, pure, bind, max, (*), (<>), (<<<), (==))
{-
-- next target is to handle this case:
var matrix = [
  [11975,  5871, 8916, 2868],
  [ 1951, 10048, 2060, 6171],
  [ 8010, 16145, 8090, 8045],
  [ 1013,   990,  940, 6907]
];

var tr = d3.select("body")
  .append("table")
  .selectAll("tr")
  .data(matrix)
  .enter().append("tr");
-}

-- | mainline: simplest possible D3 demo
array :: Array Number
array = [4.0, 8.0, 15.0, 16.0, 23.0, 42.0]

array2 :: Array String
array2 = ["awn", "bel", "cep", "dof", "erg", "fub"]

arrayMax :: Number
arrayMax = foldr max 0.0 array

revString :: String -> String
revString = fromCharArray <<< reverse <<< toCharArray

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

cep :: Number -> Index -> Nodes -> D3Element -> Boolean
cep datum _ _ _ = if (datum == 16.0) then true else false

dof :: String -> Index -> Nodes -> D3Element -> String
dof datum _ _ _ = if (datum == "erg") then "ergo propter hoc" else theHorror

hoy :: forall eff. Eff (d3::D3|eff) (Transition String)
hoy = d3Transition "ist"

main :: forall e. Eff (d3::D3,console::CONSOLE|e) Unit
main = do
  erg <- d3Transition "erg"
    .. duration 2000.0

  chart1 <- d3Select ".chart"
      .. selectAll "div"
        .. dataBind (Data array)
      .. enter .. append "div"
        .. style    "width"         (FnD (\d -> show (d * 10.0) <> "px"))
        .. classed  "twice as nice" (ClassFn (\d i nodes el -> i == 2.0 ))
        .. classed  "sixteen candles" (ClassFn cep)
        .. attr     "name"          (AttrV "zek")
        .. text                     (FnD (\d -> show d))
        .. on       mouseenter      awn
        .. on       mouseleave      awn
        .. on' click "magic" "snape" bel
        -- .. makeTransition          -- this would be a non-reusable transition example
        -- .. duration 500.0
        -- .. tStyle "background-color" (Target "#555")

  chart2 <- d3Select ".chart2"
    .. selectAll "div"
      .. dataBind (Keyed array2 (\d -> revString d))
    .. enter .. append "div"
      .. style "background-color"  (Value "red")
      .. style "width"             (FnD (\d -> show ((length d) * 20) <> "px"))
      .. classed "wis xis"         (ClassB true)
      .. attr "name"               (AttrFn dof)
      .. text                      (FnD (\d -> show d))
      .. on' click "cep" "stringy" bel

  chart1 ... savedTransition erg  -- works because the transition gets type (Transition Number) from chart1
          .. tStyle "background-color" (Target "red")
          .. tStyle "font-size"        (Target "2em")
          -- ..
-- selection.styleTween("fill", function() {
--   return function(t) {
--     return "hsl(" + t * 360 + ",100%,50%)";
--   };
-- });



  chart2 ... savedTransition erg  -- doesn't work because this needs to be (Transition String)
          .. tStyle "background-color" (Target "blue")
          .. tStyle "color"            (Target "white")

  pure unit
