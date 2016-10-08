module Main where

import D3.ForceSimulation
import Control.Monad.Eff.Console (CONSOLE)
import D3.Base (PolyValue(SetByIndex), D3, Eff, D3Element, Index, Point, AttrSetter(AttrFn, SetAttr), DataBind(Data), ListenerType(StartDrag, EndDrag, Drag), Typenames(TypeNames), (...), (..))
import D3.Drag (dragUpdate, addDragListener, d3Drag)
import D3.Scale (ScaleType(Category), scaleBy, schemeCategory20, d3Scale)
import D3.Selection (text, Selection, call, attr, append, enter, dataBind, selectAll, getAttr, d3Select, selectElem)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Pair (Pair(Pair))
import Math (sqrt)
import Prelude (Unit, unit, pure, bind, ($), (-), (/))
import Unsafe.Coerce (unsafeCoerce)

-- define a margin, look to purescript-css for more sophisticated definition
margin :: { top::Number, right::Number, bottom::Number, left::Number }
margin = { top: 20.0, right: 20.0, bottom: 30.0, left: 40.0}

foreign import miserables :: GroupedForceLayout

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

dragended :: ∀ eff. Point -> Index -> Array D3Element -> D3Element ->  Eff (d3::D3|eff) Unit
dragended d i els element = pure unit

dragstarted :: ∀ eff. Point -> Index -> Array D3Element -> D3Element ->  Eff (d3::D3|eff) Unit
dragstarted d i els element = pure unit

ticked :: ∀ eff. Selection Node -> Selection Link -> Eff (d3::D3|eff) (Eff (d3::D3|eff) Unit)
ticked node link = pure inner where
  inner :: Eff (d3::D3|eff) Unit
  inner = defaultTick node link

main :: ∀ e. Eff (d3::D3,console::CONSOLE|e) Unit
main = do
  -- get the data (to simplify this ex. to the bone, no AJAX here)
  let graph =  miserables

  svg <- d3Select ".svg"
  w   <- svg ... getAttr "width"
  h   <- svg ... getAttr "height"
  let width =  w - margin.left - margin.right
  let height = h - margin.top - margin.bottom

  color <- d3Scale (Category schemeCategory20)

  linkForce   <- makeLinkForce Nothing
                  .. setIDFunction (\d i -> d.id)
  chargeForce <- makeManyBody
  centerForce <- makeCenterForce (Just (Pair (width / 2.0) (height / 2.0)) )

  simulation <- d3ForceSimulation Force
             .. addForce Links     "link"   linkForce
             .. addForce ManyBody  "charge" chargeForce
             .. addForce Centering "center" centerForce

  link <- svg ... append "g"
      .. attr "class" (SetAttr "links")
      .. selectAll "line"
    .. dataBind (Data graph.links)
      .. enter .. append "line"
      .. attr "stroke-width" (AttrFn (\d i n e -> pure $ sqrt (d.value)))

  node <- svg ... append "g"
      .. attr "class" (SetAttr "nodes")
      .. selectAll "circle"
    .. dataBind (Data graph.nodes)
      .. enter .. append "circle"
      .. attr "r" (SetAttr 5.0)
      .. attr "fill" (AttrFn (\d i n e -> do fill <- scaleBy color d.group
                                             pure fill))

  -- seems to me that phantom type + unsafeCoerce is stupid TODO
  dragBehavior <- d3Drag { x: 0.0, y: 0.0 }
      .. addDragListener (TypeNames [ { name: Just "drag", type: Drag }]) dragged
      .. addDragListener (TypeNames [ { name: Just "end", type: EndDrag }]) dragended
      .. addDragListener (TypeNames [ { name: Just "start", type: StartDrag }]) dragstarted

  let foo = node ... call (unsafeCoerce dragBehavior)

  node ... append "title"
        .. text (SetByIndex (\d i -> pure (d.id)))

  callback <- ticked node link

  simulation ... initNodes graph.nodes
              .. onTick callback
              .. getForce "link"
              .. setLinks graph.links

  pure unit
