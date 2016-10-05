module Main where

import D3.Selection
import Miserables
import Control.Monad.Eff.Console (CONSOLE, log)
import D3.Base (Point, D3, Eff, ListenerType(..), Typenames(TypeNames), Index, D3Element, Nodes, AttrSetter(AttrFn, SetAttr), ClassSetter(SetAll, SetSome), DataBind(Keyed, Data), PolyValue(Value, SetByIndex), theHorror, (...), (..))
import D3.Drag (dragUpdate, addDragListener, d3Drag)
import D3.Interpolator (Time)
import D3.Transitions (Transition, AttrInterpolator(Target, TweenFn, TweenTarget), DelayValue(MilliSec), TransitionName(Name), tStyle, namedTransition, delay, addTransition, savedTransition, duration, d3Transition)
import D3.Zoom (scaleExtent, d3Zoom, addZoomListener, getZoomTransform)
import DOM.HTML.Event.EventTypes (mouseenter, mouseleave, click)
import Data.Array (reverse)
import Data.Foldable (foldr)
import Data.Int (floor)
import Data.Maybe (Maybe(Just))
import Data.String (length, toCharArray, fromCharArray)
import Math (sqrt)
import Prelude (class Show, Unit, show, unit, pure, bind, max, (*), (<>), (<<<), (==), ($), (+), (-), (/))
import Unsafe.Coerce (unsafeCoerce)

-- define a margin, look to purescript-css for more sophisticated definition
margin :: { top::Number, right::Number, bottom::Number, left::Number }
margin = { top: 20.0, right: 20.0, bottom: 30.0, left: 40.0}

-- initialize DOM
setup :: ∀ d eff. Eff (d3::D3|eff) (Selection d)
setup = do
          svg <- d3Select ".svg"
          w   <- svg ... getAttr "width"
          h   <- svg ... getAttr "height"
          let width =  w - margin.left - margin.right
          let height = h - margin.top - margin.bottom
          --
          -- g <- svg ... append "g"
          --   ..  attr "transform"  (SetAttr ("translate(32," <> show (height / 2.0) <> ")"))
          pure svg



main :: ∀ e. Eff (d3::D3,console::CONSOLE|e) Unit
main = do
  svg <- setup
  link <- svg ... append "g"
      .. attr "class" (SetAttr "links")
      .. selectAll "line"
    .. dataBind (Data miserables.links)
      .. enter .. append "line"
      .. attr "stroke-width" (AttrFn (\d i n e -> pure $ sqrt (d.value)))

  node <- svg ... append "g"
      .. attr "class" (SetAttr "nodes")
      .. selectAll "circle"
    .. dataBind (Data miserables.nodes)   -- go back and look at what you did in default to put multiple Selection types in one SVG

  pure unit
