module Main where

import D3.Selection
import Control.Monad.Eff.Console (CONSOLE, log)
import D3.Base (D3, Eff, D3Element, Nodes, Index, theHorror, (..), (...))
import D3.Interpolator (Time)
import D3.Transitions (DelayValue(MilliSec), delay, addTransition, tNodes, tNode, Transition, AttrInterpolator(Target, TweenFn, TweenTarget), tStyle, savedTransition, duration, d3Transition)
import DOM.HTML.Event.EventTypes (mouseenter, mouseleave, click)
import Data.Array (reverse)
import Data.Foldable (foldr)
import Data.Int (floor)
import Data.String (length, toCharArray, fromCharArray)
import Prelude (class Show, Unit, show, unit, pure, bind, max, (*), (<>), (<<<), (==), ($))
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
hoy = d3Transition "hoy"

-- ist :: Number -> Index -> D3Element -> String
ist :: forall eff. Number -> Index -> D3Element -> Eff (d3::D3|eff) String
ist d _ _ = pure $ show val <> "px" where val = d * 10.0

-- || next two functions illustrate how you can do the following JS example in PS
-- selection.styleTween("fill", function() {          // equivalent of kef
--   return function(t) {                             // equivalent of jud
--     return "hsl(" + t * 360 + ",100%,50%)";
--   };
-- });
-- kef is given as param to transition.styleTween,
-- it is called by D3 to get a customized interpolator fn for this D3Element
-- since we need it to be callable from JS, the params need to be uncurried
kef :: ∀ d eff. d -> Index -> D3Element -> Eff (d3::D3|eff) (Time -> String)
kef d i e = pure jud

-- | jud is an interpolator function given to D3 by some other interpolator-making function,
-- which is given as param to transition.styleTween (in JS it's just anonymous)
jud :: Time -> String  -- this func duplicates the D3 documentation example shown in comment below
jud t = "hsl(" <> tval <> ",100%,50%)"
  where tval = show (t * 360.0)

roc :: forall eff. String -> Index -> Eff (d3::D3|eff) String
roc d i = pure $ show d <> " " <> show i

suq :: forall d eff. Show d => d -> Index -> Eff (d3::D3|eff) String
suq d _ = pure $ show d

tej :: forall eff. String -> Index -> Eff (d3::D3|eff) String
tej d i = pure $ show l <> "px" where
  l = (length d) * 20 * (floor i)

ure :: forall eff. String -> Index -> Eff (d3::D3|eff) String
ure d _ = pure $ show d

main :: forall e. Eff (d3::D3,console::CONSOLE|e) Unit
main = do
  erg <- d3Transition "erg"
    .. duration 2000.0

  chart1 <- d3Select ".chart"
      .. selectAll "div"
        .. dataBind (Data array)
      .. enter .. append "div"
        .. style    "width"         (Value "30px")
        .. classed  "twice as nice" (SetSome (\d i nodes el -> i == 2.0 ))
        .. classed  "16 candles"    (SetSome cep)
        .. attr     "name"          (SetAttr "zek")
        .. text                     (SetByIndex suq)
        .. on       mouseenter      awn
        .. on       mouseleave      awn
        .. on' click "magic" "snape" bel
        -- .. makeTransition          -- this would be a non-reusable transition example
        -- .. duration 500.0
        -- .. tStyle "background-color" (SetAttr "#555")

  chart2 <- d3Select ".chart2"
    .. selectAll "div"
      .. dataBind (Keyed array2 (\d -> revString d))
    .. enter .. append "div"
      .. style "background-color"  (Value "red")
      .. style "width"             (SetByIndex tej)
      .. classed "wis xis"         (SetAll true)
      .. attr "name"               (AttrFn dof)
      .. text                      (SetByIndex ure)
      .. text                      (SetByIndex roc)
      .. on' click "cep" "stringy" bel

  chart1 ... savedTransition erg
          .. tStyle "color"            (Target "black")
          .. tStyle "font-size"        (Target "2em")
          .. tStyle "width"            (TweenTarget  ist)
          .. addTransition
          .. delay  (MilliSec 500.0)
          .. tStyle "background-color" (TweenFn      kef)

  chart2 ... savedTransition erg
          .. tStyle "background-color" (Target "blue")
          .. tStyle "color"            (Target "white")

  chart3 <- d3Select "notfound"

  lev <- chart2 ... node
  mim <- chart2 ... nodes

  nim <- chart3 ... node
  obi <- chart3 ... nodes
  pyx <- chart3 ... empty
  qat <- chart2 ... empty

  pure unit
