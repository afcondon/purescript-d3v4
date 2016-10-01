module D3.Collection.Maps
  ( d3MapFn
  , D3Map
  , D3MapKey
  , D3MapValue
  , D3KeyValue
  , D3KVFn
  , d3mapGetFn
  , d3mapSetFn
  , d3mapHasFn
  , d3mapRemoveFn
  , d3mapClearFn
  , d3mapKeysFn
  , d3mapValuesFn
  , d3mapEntriesFn
  , d3mapSizeFn
  , d3mapEmptyFn
  , d3mapEachFn
  ) where

import Prelude
import D3.Base (D3)
import Data.Function.Eff (EffFn2, EffFn1)

foreign import data D3Map :: * -> *

type D3MapKey   = String
type D3MapValue = Void -- d3Maps can have varying types in one map
type D3KeyValue = { key :: D3MapKey, value :: D3MapValue }
type D3KVFn     = ∀ eff. EffFn2 (d3::D3|eff) D3MapKey D3MapValue Unit

-- | Constructor for a D3Map
foreign import d3MapFn        :: ∀ d eff. EffFn1 (d3::D3|eff) d              (D3Map d)

-- | Functions available on D3Maps, similar but not same to ES5 Maps
foreign import d3mapClearFn   :: ∀ d eff. EffFn1 (d3::D3|eff) (D3Map d)          (D3Map d)
foreign import d3mapEachFn    :: ∀ d eff. EffFn2 (d3::D3|eff) D3KVFn (D3Map d)   (D3Map d)
foreign import d3mapEmptyFn   :: ∀ d eff. EffFn1 (d3::D3|eff) (D3Map d)          Boolean
foreign import d3mapEntriesFn :: ∀ d eff. EffFn1 (d3::D3|eff) (D3Map d)          (Array D3KeyValue)
foreign import d3mapGetFn     :: ∀ d eff. EffFn1 (d3::D3|eff) D3MapKey           d
foreign import d3mapHasFn     :: ∀ d eff. EffFn2 (d3::D3|eff) D3MapKey (D3Map d) Boolean
foreign import d3mapKeysFn    :: ∀ d eff. EffFn1 (d3::D3|eff) (D3Map d)          (Array D3MapKey)
foreign import d3mapRemoveFn  :: ∀ d eff. EffFn2 (d3::D3|eff) D3MapKey (D3Map d) (D3Map d)
foreign import d3mapSetFn     :: ∀ d eff. EffFn2 (d3::D3|eff) D3MapKey d         (D3Map d)
foreign import d3mapSizeFn    :: ∀ d eff. EffFn1 (d3::D3|eff) (D3Map d)          Number
foreign import d3mapValuesFn  :: ∀ d eff. EffFn1 (d3::D3|eff) (D3Map d)          (Array d)
