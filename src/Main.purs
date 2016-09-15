module Main where

import D3.Selection
import Control.Monad.Eff.Console (CONSOLE, log)
import D3.Base (D3, Eff, Index, D3Element, Nodes, AttrSetter(AttrFn, SetAttr), ClassSetter(SetAll, SetSome), DataBind(Keyed, Data), PolyValue(SetByIndex, Value), theHorror, (...), (..))
import D3.Interpolator (Time)
import D3.Transitions (Transition, AttrInterpolator(Target, TweenFn, TweenTarget), DelayValue(MilliSec), TransitionName(Name), tStyle, namedTransition, delay, addTransition, savedTransition, duration, d3Transition)
import DOM.HTML.Event.EventTypes (mouseenter, mouseleave, click)
import Data.Array (reverse)
import Data.Foldable (foldr)
import Data.Int (floor)
import Data.String (length, toCharArray, fromCharArray)
import Prelude (class Show, Unit, show, unit, pure, bind, max, (*), (<>), (<<<), (==), ($), (+))
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

type Point = { x :: Number, y :: Number }
circleData :: Array Point
circleData = [ {x: 1.0, y: 1.0}
             , {x: 2.0, y: 2.0}
             , {x: 3.0, y: 3.0}
             , {x: 4.0, y: 5.0}
             , {x: 5.0, y: 6.0}
             ]


arrayMax :: Number
arrayMax = foldr max 0.0 array

revString :: String -> String
revString = fromCharArray <<< reverse <<< toCharArray

awn :: ∀ eff. CallbackParam Number -> Eff (d3::D3, console::CONSOLE|eff) Unit
awn { datum: d, meta: m } = do
  log (show d)
  log (show m)
  pure unit

bel :: ∀ eff. CallbackParamP Number String -> Eff (d3::D3, console::CONSOLE|eff) Unit
bel { datum: d, prop: p } = do
  log (show d)
  log (show p)
  pure unit

cep :: ∀ eff. Number -> Index -> Nodes -> D3Element -> Eff (d3::D3|eff) Boolean
cep datum _ _ _ = pure $ if (datum == 16.0) then true else false

dof :: ∀ eff. String -> Index -> Nodes -> D3Element -> Eff (d3::D3|eff) String
dof datum _ _ _ = pure $ if (datum == "erg") then "ergo propter hoc" else theHorror

hoy :: ∀ eff. Eff (d3::D3|eff) (Transition String)
hoy = d3Transition (Name "hoy")

-- ist :: Number -> Index -> D3Element -> String
ist :: ∀ eff. Number -> Index -> D3Element -> Eff (d3::D3|eff) String
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

roc :: ∀ eff. String -> Index -> Eff (d3::D3|eff) String
roc d i = pure $ show d <> " " <> show i

suq :: ∀ d eff. Show d => d -> Index -> Eff (d3::D3|eff) String
suq d _ = pure $ show d

tej :: ∀ eff. String -> Index -> Eff (d3::D3|eff) String
tej d i = pure $ show l <> "px" where
  l = (length d) * 30 * (floor (i + 1.0))

ure :: ∀ eff. String -> Index -> Eff (d3::D3|eff) String
ure d _ = pure $ show d

vis :: ∀ d eff. Selection d -> String -> String -> Eff (d3::D3|eff) (Selection d)
vis s first last =
  do
    s ... attr "first-name" (SetAttr first)
      .. attr "last-name"  (SetAttr last)
    pure s
{-
  function name(selection, first, last) {
    selection
        .attr("first-name", first)
        .attr("last-name", last);
  }

  d3.selectAll("div").call(name, "John", "Snow");
-}

main :: ∀ e. Eff (d3::D3,console::CONSOLE|e) Unit
main = do
  -- | set up a named / reusable transition
  erg <- d3Transition (Name "erg")
    .. duration 2000.0

  -- | a simple chart made of `div`s from a data array of Numbers
  chartN <- d3Select ".chart"
      .. selectAll "div"
        .. dataBind (Data array)
      .. enter .. append "div"
        .. style    "width"         (Value "30px")
        .. style    "font-size"     (Value "48pt")
        .. classed  "twice as nice" (SetSome (\d i nodes el -> pure (i == 2.0) ))
        .. classed  "16 candles"    (SetSome cep)
        .. attr     "name"          (SetAttr "zek")
        .. text                     (SetByIndex suq)
        .. on       mouseenter      awn
        .. on       mouseleave      awn
        -- next an arbitrary property {prop: "propval"} is cached in the D3Element and returned in callback
        .. on' click "prop" "propval" bel
        .. call2 vis "mickey" "mouse"
        -- .. makeTransition          -- this would be a non-reusable transition example
        -- .. duration 500.0
        -- .. tStyle "background-color" (SetAttr "#555")

  -- | applying our saved transition to the chart and adding a further transition
  chartN ... savedTransition erg
          .. tStyle "color"            (Target "black")
          .. tStyle "font-size"        (Target "24pt")
          .. tStyle "width"            (TweenTarget  ist)
          .. addTransition
          .. delay  (MilliSec 500.0)
          .. tStyle "background-color" (TweenFn      kef)

  -- | a simple chart made of `div`s from a data array of Numbers
  chartS <- d3Select ".chart2"
    .. selectAll "div"
      .. dataBind (Keyed array2 (\d -> revString d))
    .. enter .. append "div"
      .. style "background-color"  (Value "red")
      .. style "width"             (SetByIndex tej)       -- 'tej' uses EffFn so it can't be presented as a lambda???
      .. classed "wis xis"         (SetAll true)
      .. attr "name"               (AttrFn dof)
      .. text                      (SetByIndex ure)
      .. text                      (SetByIndex roc)
      .. on' click "cep" "stringy" bel

  chartS ... namedTransition "erg"
          .. tStyle "background-color" (Target "blue")
          .. tStyle "color"            (Target "white")

  emptyChart <- d3Select "notfound"

  lev <- chartS ... node
  mim <- chartS ... nodes
  qat <- chartS ... empty

  nim <- emptyChart ... node
  obi <- emptyChart ... nodes
  pyx <- emptyChart ... empty

  -- | let's try some SVG stuff now so that we can work towards zooming and dragging

  svg <- d3Select ".svg"
    .. append "svg"
      .. style "width"  (Value "500px")
      .. style "height" (Value "500px")
    .. selectAll "circle"
      .. dataBind (Data circleData)
    .. enter .. append "circle"
      .. attr "cx" (AttrFn (\d i nodes el -> pure (d.x * 40.0)))
      .. attr "cy" (AttrFn (\d i nodes el -> pure (d.y * 40.0)))
      .. attr "r"  (SetAttr 20.0)
      .. style "stroke" (Value "red")
      .. style "fill"   (Value "black")

  pure unit
