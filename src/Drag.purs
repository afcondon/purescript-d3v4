module D3.Drag where

import Control.Monad.Eff (Eff)
import D3.Base (D3)
import DOM.Event.Types (Event)
import Data.Array ((:))
import Data.Foldable (intercalate, foldr)
import Data.Function.Eff (runEffFn2, runEffFn3, EffFn3, EffFn2)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Prelude (show, class Show, Unit, (<>), ($), (<$>))

foreign import data Drag :: *

foreign import d3DragFn :: ∀ eff. Eff (d3::D3|eff) Drag

-- When a drag event listener is invoked, d3.event is set to the current drag event.
foreign import d3DragEvent :: ∀ eff. Unit -> Eff (d3::D3|eff) DragEvent

foreign import data Subject :: *

type DragEvent = {
    target      :: Drag
  , type        :: String  -- either "start", "drag" or "end"
  , subject     :: Subject -- the drag subject, defined by drag.subject.
  , x           :: Number  -- the new x-coordinate of the subject
  , y           :: Number  -- the new y-coordinate of the subject
  , dx          :: Number  -- the change in x-coordinate since the previous drag event.
  , dy          :: Number  -- the change in y-coordinate since the previous drag event.
  , identifier  :: String  -- either "mouse" or numeric touch identifier
  , active      :: Number  -- the number of currently active drag gestures (on start and end, not including this one).
  , sourceEvent :: Event
}


type Draggable r = { x :: Number, y :: Number | r }  -- minimum requirement for draggable object

type D3Typenames = String

data DragType = StartType | DragType | EndType

instance isShowDragType :: Show DragType where
  show StartType = "start"
  show DragType  = "drag"
  show EndType   = "end"

foreign import data DragListener :: *

data Typenames = TypeNames (Array { name :: Maybe String, type :: DragType })

-- create a new Drag behaviour Object/Function thingy
d3Drag :: ∀ eff. Eff (d3::D3|eff) Drag
d3Drag = d3DragFn

-- smush the Typenames down to a single string which D3 will (wastefully) parse out again)
instance isShowTypenames :: Show Typenames where
  show (TypeNames s) = intercalate " " $ foldr f [] s
    where
      f {name: (Just n), type: t } acc = ((show t) <> "." <> n) : acc
      f {name: Nothing,  type: t } acc =             (show t)  : acc

-- || Going to try for a slightly richer, more PureScript-y API here just
-- because the whole polymorphic dispatch / apply on business is so confusing
-- and fluid and we're going end up making it explicit in the argument if not
-- the function so might as well bite the bullet and define some additional API
-- points
-- || i'd like to come back to this once drag and zoom are working and see whether
-- || it makes sense to wrap dispatch itself TODO

foreign import findCallbackFn    :: ∀ eff. EffFn2 (d3::D3|eff) D3Typenames                      Drag (Nullable DragListener)
foreign import removeListenersFn :: ∀ eff. EffFn2 (d3::D3|eff) D3Typenames                      Drag Drag
foreign import addListenerFn     :: ∀ eff. EffFn3 (d3::D3|eff) D3Typenames DragListener         Drag Drag

-- lookup and remove differ in JS as listeners-not-given => lookup, listeners-as-null => remove
lookupDrag      :: ∀ eff. Typenames                       -> Drag -> Eff (d3::D3|eff) (Nullable DragListener)
lookupDrag tn = runEffFn2 findCallbackFn (show tn)

removeListeners :: ∀ eff. Typenames                       -> Drag -> Eff (d3::D3|eff) Drag
removeListeners tn = runEffFn2 removeListenersFn (show tn)

addListener     :: ∀ eff. Typenames -> DragListener       -> Drag -> Eff (d3::D3|eff) Drag
addListener tn = runEffFn3 addListenerFn (show tn)

-- on :: ∀ d eff. (d -> Eff (d3::D3|eff)(unit)) -> Drag -> Eff (d3::D3|eff) Drag
-- on dragFn

{-
  drag.on = function() {
      var value = listeners.on.apply(listeners, arguments); [1]
      return value === listeners ? drag : value;
    };

// [1]
  function(typename, callback) {
      var _ = this._,
          T = parseTypenames(typename + "", _),
          t,
          i = -1,
          n = T.length;

      // If no callback was specified, return the callback of the given type and name.
      if (arguments.length < 2) {
        while (++i < n) if ((t = (typename = T[i]).type) && (t = get(_[t], typename.name))) return t;
        return;
      }

      // If a type was specified, set the callback for the given type and name.
      // Otherwise, if a null callback was specified, remove callbacks of the given name.
      if (callback != null && typeof callback !== "function") throw new Error("invalid callback: " + callback);
      while (++i < n) {
        if (t = (typename = T[i]).type) _[t] = set$2(_[t], typename.name, callback);
        else if (callback == null) for (t in _) _[t] = set$2(_[t], typename.name, null);
      }

      return this;
    }

  -}

type AccessorFn d = ∀ eff. (d -> Eff (d3::D3|eff) Unit)

-- data SubjectOpts d = GetAccessor
--                    | SetAccessorFn (d -> ?)
--                    | SetAccessorObj Draggable
{-
  The subject of a drag gesture represents the thing being dragged. It is computed
  when an initiating input event is received, such as a mousedown or touchstart,
  immediately before the drag gesture starts. The subject is then exposed as
  event.subject on subsequent drag events for this gesture.
-}

-- if subject specified sets the subject accessor to the specified object or function and returns the drag behavior.
-- subject :: ∀ d eff. SubjectOpts d ->

-- If subject is not specified, returns the current subject accessor, which defaults to:
{-
    function subject(d) {
      return d == null ? {x: event.x, y: event.y} : d;
    }
-}


-- sets the filter to the specified function and returns the drag behavior.
-- filter ::

-- sets the container accessor to the specified object or function and returns the drag behavior
-- container ::
