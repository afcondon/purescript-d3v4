module Main where

import Control.Monad.Eff (Eff)
import Data.Function.Eff (EffFn3, EffFn2, EffFn1, runEffFn3, runEffFn1, runEffFn2)
import Prelude (show, bind, (<>), (*), max)
import Data.Foldable (foldr)

-- || FFI stuff
foreign import rootSelectImpl     :: forall d eff. EffFn1 (d3::D3|eff) String                                       (Selection d)
foreign import selectAllImpl      :: forall d eff. EffFn2 (d3::D3|eff) String                         (Selection d) (Selection d)
foreign import bindDataImpl       :: forall d eff. EffFn2 (d3::D3|eff) (Array d)                      (Selection d) (Selection d)
foreign import enterImpl          :: forall d eff. EffFn1 (d3::D3|eff)                                (Selection d) (Selection d)

foreign import unsafeAppendImpl   :: forall d eff. EffFn2 (d3::D3|eff) String                         (Selection d) (Selection d)
foreign import unsafeInsertImpl   :: forall d eff. EffFn2 (d3::D3|eff) String                         (Selection d) (Selection d)

foreign import unsafeStyleImpl    :: forall d v eff. EffFn3 (d3::D3|eff) String v                       (Selection d) (Selection d)
foreign import unsafeStyleImplP   :: forall d v eff. EffFn3 (d3::D3|eff) String (v -> String)           (Selection d) (Selection d)
foreign import unsafeStyleImplPP  :: forall d v eff. EffFn3 (d3::D3|eff) String (v -> Number -> String) (Selection d) (Selection d)

foreign import unsafeTextImpl     :: forall d v eff. EffFn2 (d3::D3|eff) v                              (Selection d) (Selection d)
foreign import unsafeTextImplP    :: forall d v eff. EffFn2 (d3::D3|eff) (v -> String)                  (Selection d) (Selection d)
foreign import unsafeTextImplPP   :: forall d v eff. EffFn2 (d3::D3|eff) (v -> Number -> String)        (Selection d) (Selection d)

-- | originally from Graphics.D3.Base
foreign import data D3 :: !
type D3Eff a = forall e. Eff (d3 :: D3 | e) a
foreign import data D3Element :: *
-- | originally from Graphics.D3.Util
infixl 4 bind as ..        -- (..) = (>>=)
-- | originally from Graphics.D3.Selection
foreign import data Selection :: * -> *

-- | basis for new typeclass-less Graphics.D3.Selection
data PolyValue a  = Value a
                  | FnD  (a -> String)
                  | FnDI (a -> Number -> String)

style  :: forall d a eff.  String -> PolyValue a -> (Selection d) -> Eff (d3::D3|eff) (Selection d)
style name (Value value) = runEffFn3 unsafeStyleImpl name value
style name (FnD  f)      = runEffFn3 unsafeStyleImplP name f
style name (FnDI f)      = runEffFn3 unsafeStyleImplPP name f

text  :: forall d a eff.  PolyValue a -> (Selection d) -> Eff (d3::D3|eff) (Selection d)
text (Value value) = runEffFn2 unsafeTextImpl value
text (FnD  f)      = runEffFn2 unsafeTextImplP f
text (FnDI f)      = runEffFn2 unsafeTextImplPP f

rootSelect :: forall d eff. String -> Eff (d3::D3|eff) (Selection d)
rootSelect selector = runEffFn1 rootSelectImpl selector

selectAll :: forall d eff. String -> Selection d -> Eff (d3::D3|eff) (Selection d)
selectAll selector = runEffFn2 selectAllImpl selector

bindData :: forall d eff. Array d -> Selection d -> Eff (d3::D3|eff) (Selection d)
bindData = runEffFn2 bindDataImpl

enter :: forall d eff. Selection d -> Eff (d3::D3|eff) (Selection d)
enter = runEffFn1 enterImpl

insert  :: forall d eff.  String ->               Selection d -> Eff (d3::D3|eff) (Selection d)
insert tag = runEffFn2 unsafeInsertImpl tag

append  :: forall d eff.  String ->               Selection d -> Eff (d3::D3|eff) (Selection d)
append tag = runEffFn2 unsafeAppendImpl tag


-- | mainline: simplest possible D3 demo
array :: Array Number
array = [4.0, 8.0, 15.0, 16.0, 23.0, 42.0]

arrayMax :: Number
arrayMax = foldr max 0.0 array

myShow :: Number -> String
myShow n = show n

main :: forall e. Eff ( d3 :: D3 | e ) (Selection Number)
main = do
  rootSelect ".chart"
    .. selectAll "div"
      .. bindData array
    .. enter .. append "div"
      .. style "width" (FnD (\d -> show (d * 10.0) <> "px"))
      .. text (FnD myShow)
