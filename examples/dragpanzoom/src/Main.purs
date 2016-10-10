module Main where

import D3.Selection (call, style, attr, append, enter, dataBind, selectAll, d3Select, selectElem)
import D3.Base (D3, Eff, D3Element, Index, Point, AttrSetter(..), DataBind(..), ListenerType(..), PolyValue(..), Typenames(..), (...), (..))
import D3.Drag (addDragListener, d3Drag, dragUpdate)
import D3.Zoom (addZoomListener, scaleExtent, d3Zoom, getZoomEvent)
import Prelude (Unit, unit, pure, bind)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)

circleData :: Array Point
circleData = [ {x: 100.0, y: 100.0}
             , {x: 200.0, y: 200.0}
             , {x: 100.0, y: 200.0}
             , {x: 200.0, y: 100.0}
             , {x: 150.0, y: 150.0}
             ]

-- an example of a drag listener written in Purescript
-- element will track pointer / finger, but other possibilities exist such as
-- faster or slower than dragging or adding acceleration or further side-effects
dragged :: ∀ eff. Point -> Index -> Array D3Element -> D3Element ->  Eff (d3::D3|eff) Unit
dragged d i els element = do
  selectElem element
    .. attr "cx" (SetAttr d.x) -- not changing the underlying datum here
    .. attr "cy" (SetAttr d.y) -- so nothing happens. Add further Attr options? TODO
  dragUpdate d element -- state mutating function from drag.purs that makes the change
  pure unit

-- an example of a zoom listener written in Purescript
-- this just gives the most naive implementation but here's where you'd begin to implement
-- semantic zooms and the like
-- in the example he's using a global var for the <g> which actually gets transformed
-- but we're (for now) just going to look it up each time
zoomed :: ∀ d eff. d -> Index -> Array D3Element -> D3Element ->  Eff (d3::D3|eff) Unit
zoomed d i els element = do
  g  <- d3Select "g"

  zt <- getZoomEvent

  attr "transform" (SetAttr zt.transform) g

  pure unit

main :: ∀ e. Eff (d3::D3,console::CONSOLE|e) Unit
main = do
  -- the containing SVG
  svg <- d3Select ".svg"
  -- appending a "g" to the svg
  g <-  svg ... append "g"
  -- creating a zoomBehavior
  zoomBehavior <- d3Zoom
             .. scaleExtent [ 0.5, 8.0]
             .. addZoomListener (TypeNames [ { name: Just "zoom.bar", type: Zoom } ]) zoomed
  -- attaching the zoomBehavior to the svg so that zoom events will fire the "zoomed" fn
  let bar = svg ... call (unsafeCoerce zoomBehavior)

  -- next, entering the the data-driven part, one "circle" for every datum
  circles <- g ... selectAll "circle"
      .. dataBind (Data circleData)
    .. enter .. append "circle"
      .. attr "cx" (AttrFn (\d i nodes el -> pure d.x)) -- thing to bear in mind here:
      .. attr "cy" (AttrFn (\d i nodes el -> pure d.y)) -- if you mod here doesn't change underlying value when you drag
      .. attr "r"  (SetAttr 20.0)
      .. style "stroke" (Value "red")
      .. style "fill"   (Value "black")

  -- phantom type for d3Drag to ensure correct type for dragBehavior (but type only gets in the way here, potentially)
  dragBehavior <- d3Drag { x: 0.0, y: 0.0 }
               .. addDragListener (TypeNames [ { name: Just "drag.foo", type: Drag } ]) dragged
  -- adds the drag callbacks for drag (dragBehavior) on selection (g)
  let foo = circles ... call (unsafeCoerce dragBehavior)
  -- unsafeCoerce here is obviously undesirable, need to play with types and see if we can reformulate to lose it TODO

  pure unit
