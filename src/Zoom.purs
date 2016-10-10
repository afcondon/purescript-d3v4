module D3.Zoom (
      Zoom
    , ZoomEvent
    , Transform
    , Extent
    , d3Zoom
    , getZoomTransform
    , getZoomEvent
    , scaleExtent
    , addZoomListener
  ) where

import D3.Base (D3Element, D3Typenames, D3, Eff, Typenames)
import D3.Drag (EffFn3PlusThis, DragListener, mkEffFn4Special)
import DOM.Event.Types (Event)
import Data.Function.Eff (EffFn2, EffFn3, runEffFn2, runEffFn3)
import Prelude (Unit, show)

foreign import data Zoom :: *

type Extent = Array Number -- of `length` 2, just the min and max zoom

-- When a zoom event listener is invoked, d3.event is set to the current drag event.
foreign import d3ZoomEventFn  :: ∀ eff. Eff (d3::D3|eff) ZoomEvent
foreign import getTransformFn :: ∀ eff. Eff (d3::D3|eff) Transform

type Transform = { k :: Number, x :: Number, y:: Number }
type ZoomEvent = {
    target      :: Zoom
  , type        :: String
  , transform   :: Transform
  , sourceEvent :: Event
}

foreign import d3ZoomFn       :: ∀ eff. Eff    (d3::D3|eff)             Zoom
foreign import scaleExtentFn  :: ∀ eff. EffFn2 (d3::D3|eff) Extent Zoom Zoom
foreign import addZoomListenerFn  :: ∀ d eff. EffFn3 (d3::D3|eff) D3Typenames
                                                              (EffFn3PlusThis (d3::D3|eff) d Number (Array D3Element) Unit)
                                                              Zoom
                                                              Zoom

d3Zoom :: ∀ eff. Eff (d3::D3|eff) Zoom
d3Zoom = d3ZoomFn

scaleExtent :: ∀ eff. Extent -> Zoom -> Eff (d3::D3|eff) Zoom
scaleExtent = runEffFn2 scaleExtentFn

addZoomListener :: ∀ d eff. Typenames -> DragListener d -> Zoom -> Eff (d3::D3|eff) Zoom
addZoomListener tn callback = runEffFn3 addZoomListenerFn (show tn) (mkEffFn4Special callback)

getZoomTransform :: ∀ eff. Eff (d3::D3|eff) Transform
getZoomTransform = getTransformFn

getZoomEvent :: ∀ eff. Eff (d3::D3|eff) ZoomEvent
getZoomEvent = d3ZoomEventFn
