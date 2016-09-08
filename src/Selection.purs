module D3.Selection
  ( Selection         -- Types
  , AttrSetter(..)
  , ClassSetter(..)
  , DataBind(..)
  , PolyValue(..)
  , CallbackParam
  , CallbackParamP
  , d3Select         -- functions that yield a selection
  , d3SelectAll
  , append           -- functions belonging to selections
  , attr
  -- , call
  , classed
  , dataBind
  -- , each
  -- , empty
  , enter
  , exit
  , insert
  -- , merge
  -- , node
  -- , nodes
  , on
  , on'             -- revisit this and see if can be wrapped in ADT like the other polymorphic functions TODO
  , remove
  , select
  , selectAll
  , selectElem
  , size
  , style
  , text
  ) where

import DOM.Event.Types (EventType)
import Data.Function.Eff (EffFn5, EffFn3, EffFn2, EffFn1, runEffFn3, runEffFn5, runEffFn1, runEffFn2)
import Prelude (Unit)

import D3.Base

foreign import data Selection :: * -> *

foreign import appendFn     :: forall d eff.      EffFn2 (d3::D3|eff) String                      (Selection d) (Selection d)
foreign import bindDataFn   :: forall d eff.      EffFn2 (d3::D3|eff) (Array d)                   (Selection d) (Selection d)
foreign import bindDataFnK  :: forall d k eff.    EffFn3 (d3::D3|eff) (Array d) (d -> k)          (Selection d) (Selection d)
foreign import classedFn    :: forall d eff.      EffFn3 (d3::D3|eff) String Boolean              (Selection d) (Selection d)
foreign import classedFnP   :: forall d eff.      EffFn3 (d3::D3|eff) String (PredicateB d)       (Selection d) (Selection d)
foreign import d3SelectAllFn :: forall d eff.     EffFn1 (d3::D3|eff) String                                    (Selection d)
foreign import d3SelectFn   :: forall d eff.      EffFn1 (d3::D3|eff) String                                    (Selection d)
foreign import enterFn      :: forall d eff.      EffFn1 (d3::D3|eff)                             (Selection d) (Selection d)
foreign import exitFn       :: forall d eff.      EffFn1 (d3::D3|eff)                             (Selection d) (Selection d)
foreign import filterFn     :: forall d eff.      EffFn2 (d3::D3|eff) String                      (Selection d) (Selection d)
foreign import filterFnP    :: forall d eff.      EffFn2 (d3::D3|eff) (d -> Boolean)              (Selection d) (Selection d)
foreign import insertFn     :: forall d eff.      EffFn2 (d3::D3|eff) String                      (Selection d) (Selection d)
foreign import orderFn      :: forall d eff.      EffFn1 (d3::D3|eff)                             (Selection d) (Selection d)
foreign import removeFn     :: forall d eff.      EffFn1 (d3::D3|eff)                             (Selection d) (Selection d)
foreign import selectAllFn  :: forall d eff.      EffFn2 (d3::D3|eff) String                      (Selection d) (Selection d)
foreign import selectElFn   :: forall d eff.      EffFn1 (d3::D3|eff) D3Element                                 (Selection d)
foreign import selectFn     :: forall d eff.      EffFn2 (d3::D3|eff) String                      (Selection d) (Selection d)
foreign import styleFn      :: forall d v eff.    EffFn3 (d3::D3|eff) String v                    (Selection d) (Selection d)
foreign import styleFnP     :: forall d v v2 eff. EffFn3 (d3::D3|eff) String (v -> v2)            (Selection d) (Selection d)
foreign import styleFnPP    :: forall d v v2 eff. EffFn3 (d3::D3|eff) String (v -> Number -> v2)  (Selection d) (Selection d)
foreign import textFn       :: forall d v eff.    EffFn2 (d3::D3|eff) v                           (Selection d) (Selection d)
foreign import textFnP      :: forall d v v2 eff. EffFn2 (d3::D3|eff) (v -> v2)                   (Selection d) (Selection d)
foreign import textFnPP     :: forall d v v2 eff. EffFn2 (d3::D3|eff) (v -> Number -> v2)         (Selection d) (Selection d)
foreign import attrFn       :: forall d v eff.    EffFn3 (d3::D3|eff) String v                    (Selection d) (Selection d)
foreign import attrFnP      :: forall d v eff.    EffFn3 (d3::D3|eff) String (PredicateFn v d)    (Selection d) (Selection d)
foreign import sizeFn       :: forall d eff.      EffFn1 (d3::D3|eff)                             (Selection d) Int

-- | ADT used to wrap those polymorphic calls in D3 which take either
--      a value, or...
--      a function to get a value from the datum, or...
--      a function to get a value from the datum and its index
data DataBind d k = Data (Array d)
                  | Keyed (Array d) (d -> k)

data PolyValue d v  = Value v
                    | FnD  (d -> v)
                    | FnDI (d -> Number -> v)

data Filter d       = Selector  String
                    | Predicate (d -> Boolean)

data ClassSetter  d = ClassB Boolean
                    | ClassFn (PredicateB d)

data AttrSetter v d = AttrV v
                    | AttrFn (PredicateFn v d)

classed :: forall d eff. String -> ClassSetter d    -> Selection d -> Eff (d3::D3|eff) (Selection d)
classed s (ClassB b)  = runEffFn3 classedFn  s b
classed s (ClassFn p) = runEffFn3 classedFnP s p

attr :: forall v d eff. String -> AttrSetter v d    -> Selection d -> Eff (d3::D3|eff) (Selection d)
attr s (AttrV b)  = runEffFn3 attrFn  s b
attr s (AttrFn p) = runEffFn3 attrFnP s p

style  :: forall d v eff.  String -> PolyValue d v  -> Selection d -> Eff (d3::D3|eff) (Selection d)
style name (Value value)  = runEffFn3 styleFn name value
style name (FnD  f)       = runEffFn3 styleFnP name f
style name (FnDI f)       = runEffFn3 styleFnPP name f

text  :: forall d v eff.  PolyValue d v             -> Selection d -> Eff (d3::D3|eff) (Selection d)
text       (Value value)  = runEffFn2 textFn value
text       (FnD  f)       = runEffFn2 textFnP f
text       (FnDI f)       = runEffFn2 textFnPP f

d3Select :: forall d eff. String                                    -> Eff (d3::D3|eff) (Selection d)
d3Select selector         = runEffFn1 d3SelectFn selector

d3SelectAll :: forall d eff. String                                 -> Eff (d3::D3|eff) (Selection d)
d3SelectAll selector      = runEffFn1 d3SelectAllFn selector

selectAll :: forall d eff. String                    -> Selection d -> Eff (d3::D3|eff) (Selection d)
selectAll selector        = runEffFn2 selectAllFn selector

selectElem :: forall d eff. D3Element                                  -> Eff (d3::D3|eff) (Selection d)
selectElem element           = runEffFn1 selectElFn element

select  :: forall d eff.  String                     -> Selection d -> Eff (d3::D3|eff) (Selection d)
select selector           = runEffFn2 selectFn selector

dataBind :: forall d k eff. DataBind d k    -> Selection d -> Eff (d3::D3|eff) (Selection d)
-- would be nice to express that Keyed needs k to be Ord, will have to wait for GADTs
-- dataBind :: forall d k eff. Ord k => DataBind d k    -> Selection d -> Eff (d3::D3|eff) (Selection d)
dataBind (Data dataArray)         = runEffFn2 bindDataFn dataArray
dataBind (Keyed dataArray keyFn)  = runEffFn3 bindDataFnK dataArray keyFn

filter  :: forall d eff.  Filter d                   -> Selection d -> Eff (d3::D3|eff) (Selection d)
filter (Selector s)       = runEffFn2 filterFn s
filter (Predicate p)      = runEffFn2 filterFnP p

order :: forall d eff.                                  Selection d -> Eff (d3::D3|eff) (Selection d)
order                     = runEffFn1 orderFn

enter :: forall d eff.                                  Selection d -> Eff (d3::D3|eff) (Selection d)
enter                     = runEffFn1 enterFn

exit :: forall d eff.                                   Selection d -> Eff (d3::D3|eff) (Selection d)
exit                      = runEffFn1 exitFn

remove :: forall d eff.                                 Selection d -> Eff (d3::D3|eff) (Selection d)
remove                    = runEffFn1 removeFn

size :: forall d eff.                                   Selection d -> Eff (d3::D3|eff) Int
size                      = runEffFn1 sizeFn

insert  :: forall d eff.  String                     -> Selection d -> Eff (d3::D3|eff) (Selection d)
insert tag                = runEffFn2 insertFn tag

append  :: forall d eff.  String                     -> Selection d -> Eff (d3::D3|eff) (Selection d)
append tag                = runEffFn2 appendFn tag

-- || Callback stuff
-- first up from Graphics.D3.EffFnExtra
type PropertyName = String
type CallbackParam d =
    { datum     :: d
    , elem      :: D3Element
    , timestamp :: Number
    , meta      :: Boolean
    , shift     :: Boolean
    , ctrl      :: Boolean
    , alt       :: Boolean
  }
type CallbackParamP d p =
    { datum     :: d
    , elem      :: D3Element
    , timestamp :: Number
    , prop      :: p
    , meta      :: Boolean
    , shift     :: Boolean
    , ctrl      :: Boolean
    , alt       :: Boolean
  }

foreign import data D3EffCallback      :: # ! -> * -> * -> *
foreign import data D3EffCallbackP     :: # ! -> * -> * -> * -> *
foreign import      mkCallback         :: forall eff d r.   (CallbackParam d -> Eff eff r)
  -> D3EffCallback eff (CallbackParam d) r
foreign import      mkCallbackWithProp :: forall eff d p r. (CallbackParamP d p -> Eff eff r) -> PropertyName
  -> D3EffCallbackP eff (CallbackParamP d p) PropertyName r

foreign import onFn :: forall eff a d.
    EffFn3 (d3::D3|eff)
      (Selection a)               -- 1st argument for EffFn3, the selection itself
      EventType                   -- 2nd argument for EffFn3, the type of the event being bound
      (D3EffCallback (d3::D3|eff) -- 3rd argument for EffFn3, this is the callback function
        (CallbackParam d)           -- arg for callback mkCallback
        Unit)                       -- result of mkCallback
      (Selection a)               -- result of EffFn3, returns selection for "fluid interface" / monadic chain

foreign import onFnWithProperty :: forall eff a d p.
  EffFn5 (d3::D3|eff)
        (Selection a)               -- 1st argument for EffFn5, the selection itself
        EventType                   -- 2nd argument for EffFn5, the type of the event being bound
        (D3EffCallbackP (d3::D3|eff)-- 3rd argument for EffFn5, this is the callback function
            (CallbackParamP d p)      -- arg 1 for callback mkCallbackWithProp,
            PropertyName              -- arg 2 for callback mkCallbackWithProp
            Unit)                     --  result of mkCallbackWithProp
        PropertyName                -- 4th argument for EffFn5, name of a property to cache something in
        p                           -- 5th argument for EffFn5, something to cache in the property field
        (Selection a)               -- result of EffFn5, returns selection for "fluid interface" / monadic chain

-- generic "on" function works for any DOM event
on :: forall a d eff. EventType
                -> (CallbackParam d -> Eff (d3::D3|eff) Unit)
                -> (Selection a) -> Eff (d3::D3|eff) (Selection a)
on event callback selection  = runEffFn3 onFn selection event (mkCallback callback)

on' :: forall a d p eff. EventType -> PropertyName -> p
                -> (CallbackParamP d p -> Eff (d3::D3|eff) Unit)
                -> (Selection a) -> Eff (d3::D3|eff) (Selection a)
on' evType propName prop callback sel = runEffFn5 onFnWithProperty sel evType (mkCallbackWithProp callback propName) propName prop
