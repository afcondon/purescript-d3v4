module D3.Event where


import Control.Monad.Eff (Eff)
import D3.Base (D3)
import DOM.Event.Types (Event, TouchEvent)
import DOM.HTML.Types (HTMLElement)
import Data.Function.Eff (runEffFn3, runEffFn1, EffFn3, EffFn1, EffFn2, runEffFn2)
import Data.Nullable (Nullable)
import Prelude (Unit)

foreign import d3EventFn :: ∀ eff. Unit -> Eff (d3::D3|eff) Event

type D3Point = Array Number -- in reality only two elements [x,y]

type HTMLContainer = HTMLElement -- see no way of constraining this at the moment, i think

-- gets the {x,y} relative to a specified container (such as HTML or SVG element)
foreign import d3MouseFn :: ∀ eff. HTMLContainer -> Eff (d3::D3|eff) D3Point

-- || NB all this touch stuff is very sketchy and unimplemented

-- Returns the x and y coordinates of the touch with the specified identifier
-- associated with the current event relative to the specified container.
type D3Touch      = TouchEvent -- not sure that this is right, but it can be a placeholder
type TouchID      = String
type TouchHistory = Array (Array Number)   -- of the form  [[x1,y1], [x2,y2]]

foreign import d3TouchFn          :: ∀ eff. EffFn3 (d3::D3|eff) HTMLContainer (Array D3Touch) TouchID    (Nullable TouchHistory)
foreign import d3TouchDefaultFn   :: ∀ eff. EffFn2 (d3::D3|eff) HTMLContainer                 TouchID    (Nullable TouchHistory)
foreign import d3TouchesFn        :: ∀ eff. EffFn2 (d3::D3|eff) HTMLContainer (Array D3Touch)            TouchHistory
foreign import d3TouchesDefaultFn :: ∀ eff. EffFn1 (d3::D3|eff) HTMLContainer                            TouchHistory

-- # d3.touch(container[, touches], identifier)
d3Touch :: ∀ eff. HTMLContainer -> Array D3Touch -> TouchID -> Eff (d3::D3|eff) (Nullable TouchHistory)
d3Touch = runEffFn3 d3TouchFn

d3TouchDefault :: ∀ eff. HTMLContainer -> TouchID           -> Eff (d3::D3|eff) (Nullable TouchHistory)
d3TouchDefault = runEffFn2 d3TouchDefaultFn

-- # d3.touches(container[, touches])
d3Touches        :: ∀ eff. HTMLContainer -> Array D3Touch -> Eff (d3::D3|eff) TouchHistory
d3Touches        = runEffFn2 d3TouchesFn

d3TouchesDefault :: ∀ eff. HTMLContainer                  -> Eff (d3::D3|eff) TouchHistory
d3TouchesDefault = runEffFn1 d3TouchesDefaultFn
