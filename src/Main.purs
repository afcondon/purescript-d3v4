module Main where

import Control.Monad.Eff (Eff)
import Data.Function.Eff (EffFn3, EffFn2, EffFn1, runEffFn3, runEffFn1, runEffFn2)
import Prelude (show, id, bind, (<>), (*), max)
import Data.Foldable (foldr)
-- import Graphics.D3.Base (D3)
-- import Graphics.D3.Util ((..))
-- import Graphics.D3.Selection (Selection, text', style', append, enter, bindData, selectAll, rootSelect)

-- || FFI stuff
foreign import rootSelectImpl     :: forall eff.       EffFn1 (d3::D3|eff) String                                  (Selection Void)
foreign import selectAllImpl      :: forall s eff.     EffFn2 (d3::D3|eff) String s                                 s
foreign import bindDataImpl       :: forall o n eff.   EffFn2 (d3::D3|eff) (Array n) (Selection o)                 (Update n)
foreign import enterImpl          :: forall d eff.     EffFn1 (d3::D3|eff) (Update d)                              (Enter d)
foreign import unsafeStyleImplP   :: forall d s eff.   EffFn3 (d3::D3|eff) String (d -> String) s                   s
foreign import unsafeTextImplP    :: forall d s eff.   EffFn2 (d3::D3|eff) (d -> String) s                          s
foreign import unsafeAppendImpl   :: forall x s eff.   EffFn2 (d3::D3|eff) String x                                 s
foreign import unsafeInsertImpl   :: forall x s eff.   EffFn2 (d3::D3|eff) String x                                 s

-- | from Graphics.D3.Base
foreign import data D3 :: !
type D3Eff a = forall e. Eff (d3 :: D3 | e) a
foreign import data D3Element :: *
-- | from Graphics.D3.Util
infixl 4 bind as ..        -- (..) = (>>=)
-- | from Graphics.D3.Selection
foreign import data Selection :: * -> *
foreign import data Update :: * -> *
foreign import data Enter :: * -> *

-- The (uninhabited) type of an unbound selection's data
data Void

rootSelect :: forall eff. String -> Eff (d3::D3|eff) (Selection Void)
rootSelect selector
  = runEffFn1 rootSelectImpl selector

unsafeSelectAll :: forall s eff. String -> s -> Eff (d3::D3|eff) s
unsafeSelectAll selector
  = runEffFn2 selectAllImpl selector

bindData      :: forall od nd eff. Array nd -> Selection od -> Eff (d3::D3|eff) (Update nd)
bindData
  = runEffFn2 bindDataImpl

enter :: forall d eff. Update d -> Eff (d3::D3|eff) (Enter d)
enter = runEffFn1 enterImpl

unsafeStyle'  :: forall d s eff.  String -> (d -> String) -> s -> Eff (d3::D3|eff) s
unsafeStyle' name fn_d -- function using datum to produce value
  = runEffFn3 unsafeStyleImplP name fn_d

unsafeText'   :: forall d s eff.  (d -> String) -> s -> Eff (d3::D3|eff) s
unsafeText' fn_d
  = runEffFn2 unsafeTextImplP fn_d

unsafeInsert  :: forall x y eff.  String -> x -> Eff (d3::D3|eff) y
unsafeInsert tag
  = runEffFn2 unsafeInsertImpl tag

unsafeAppend  :: forall x y eff.  String -> x -> Eff (d3::D3|eff) y
unsafeAppend tag
  = runEffFn2 unsafeAppendImpl tag

-- Selection-y things that contain existing DOM elements
class Existing s where
  selectAll :: forall d eff.   String ->                              s d -> Eff (d3::D3|eff) (s d)
  style'    :: forall d eff.   String -> (d -> String) ->             s d -> Eff (d3::D3|eff) (s d)
  text'     :: forall d eff.             (d -> String) ->             s d -> Eff (d3::D3|eff) (s d)

instance existingSelection :: Existing Selection where
  selectAll = unsafeSelectAll
  style'    = unsafeStyle'
  text'     = unsafeText'

instance existingUpdate :: Existing Update where
  selectAll = unsafeSelectAll
  style'    = unsafeStyle'
  text'     = unsafeText'

-- Selection-y things which can be appended to / inserted into
class Appendable s where
  append :: forall d eff. String -> s d -> Eff (d3::D3|eff) (Selection d)
  insert :: forall d eff. String -> s d -> Eff (d3::D3|eff) (Selection d)

instance appendableSelection  :: Appendable Selection where
  append = unsafeAppend
  insert = unsafeInsert

instance appendableUpdate     :: Appendable Update where
  append = unsafeAppend
  insert = unsafeInsert

instance appendableEnter      :: Appendable Enter where
  append = unsafeAppend
  insert = unsafeInsert








array :: Array Number
array = [4.0, 8.0, 15.0, 16.0, 23.0, 42.0]

arrayMax = foldr max 0.0 array

main :: forall e. Eff ( d3 :: D3 | e ) (Selection Number)
main = do
  rootSelect ".chart"
    .. selectAll "div"
      .. bindData array
    .. enter .. append "div"
      .. style' "width" (\d -> show (d * 10.0) <> "px")
      .. text' show
