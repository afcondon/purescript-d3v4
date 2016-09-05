module Main where

import Control.Monad.Eff (Eff)
import Prelude (show, id, bind, (<>))
import Graphics.D3.Base (D3)
import Graphics.D3.Util (maxFn, (..))
import Graphics.D3.Selection (Selection, text', style', append, enter, bindData, selectAll, rootSelect)
import Graphics.D3.Scale (toFunction, range, domain, linearScale)

-- | This is a PureScript adaptation of part 1 of Mike Bostock's "Let's Make a Bar Chart" series:
-- | http://bost.ocks.org/mike/bar/1/

{-
Original JavaScript code:
=========================
var data = [4, 8, 15, 16, 23, 42];
var x = d3.scale.linear()
    .domain([0, d3.max(data)])
    .range([0, 420]);
d3.select(".chart")
  .selectAll("div")
    .data(data)
  .enter().append("div")
    .style("width", function(d) { return x(d) + "px"; })
    .text(function(d) { return d; });
-}

array :: Array Number
array = [4.0, 8.0, 15.0, 16.0, 23.0, 42.0]

-- main :: forall e. Eff (d3 :: D3, dom :: DOM, console :: CONSOLE | e) Unit
main :: forall e. Eff ( d3 :: D3 | e ) (Selection Number)
main = do
  x <- linearScale
    .. domain [0.0, maxFn id array]
    .. range [0.0, 420.0]
    .. toFunction

  rootSelect ".chart"
    .. selectAll "div"
      .. bindData array
    .. enter .. append "div"
      .. style' "width" (\d -> show (x d) <> "px")
      .. text' show
