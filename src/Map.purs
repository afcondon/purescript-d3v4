module D3.Collections.Map
  ( d3MapFn
  , D3Map
  , D3MapKey
  -- , D3MapValue
  , D3KeyValue
  , D3KVFn
  , d3Map
  , d3MapF
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
import Control.Monad.Eff (Eff)
import D3.Base (D3)
import Data.Function.Eff (EffFn2, EffFn1, runEffFn1, runEffFn2)

foreign import data D3Map :: * -> *

type D3MapKey     = String
-- type D3MapValue   = Void -- d3Maps can have varying types in one map
type D3KeyValue d  = { key :: D3MapKey, value :: d }
type D3KVFn       = ∀ d eff. EffFn2 (d3::D3|eff) D3MapKey d Unit
-- type D3MakeKeysEffFn d = ∀ eff. EffFn1 (d3::D3|eff) d          D3MapKey
-- type D3MakeKeysFn d    = ∀ eff. d          -> Eff (d3::D3|eff) D3MapKey
--
-- test :: ∀ d eff. D3MakeKeysFn d -> D3MakeKeysEffFn d
-- test = mkEffFn1
--
-- test' :: ∀ d eff. D3MakeKeysEffFn d -> D3MakeKeysFn d
-- test' = runEffFn1

-- | Constructor for a D3Map that takes a collection and tries to make Map from it
-- | since this can fail it would be good to add EXCEPTION here and return EITHER - TODO
foreign import d3MapFn        :: ∀ c d eff. EffFn1 (d3::D3|eff) c                (D3Map d)
foreign import d3MapFnFn      :: ∀ c d eff. EffFn2 (d3::D3|eff)
                                                    c
                                                    (d -> D3MapKey) -- function passed in
                                                    (D3Map d)


-- | Constructor for a D3Map that takes a collection and a function to produce keys from the data
-- foreign import d3MapFnFn      :: ∀ c d eff. EffFn2 (d3::D3|eff) c  (D3MakeKeysEffFn d)  (D3Map d)

-- | Functions available on D3Maps, similar but not same to ES5 Maps
foreign import d3mapClearFn   :: ∀ d eff. EffFn1 (d3::D3|eff) (D3Map d)          (D3Map d)
foreign import d3mapEmptyFn   :: ∀ d eff. EffFn1 (d3::D3|eff) (D3Map d)          Boolean
foreign import d3mapEntriesFn :: ∀ d eff. EffFn1 (d3::D3|eff) (D3Map d)          (Array (D3KeyValue d))
foreign import d3mapGetFn     :: ∀ d eff. EffFn1 (d3::D3|eff) D3MapKey           d
foreign import d3mapKeysFn    :: ∀ d eff. EffFn1 (d3::D3|eff) (D3Map d)          (Array D3MapKey)
foreign import d3mapSizeFn    :: ∀ d eff. EffFn1 (d3::D3|eff) (D3Map d)          Number
foreign import d3mapValuesFn  :: ∀ d eff. EffFn1 (d3::D3|eff) (D3Map d)          (Array d)
foreign import d3mapHasFn     :: ∀ d eff. EffFn2 (d3::D3|eff) D3MapKey (D3Map d) Boolean
foreign import d3mapRemoveFn  :: ∀ d eff. EffFn2 (d3::D3|eff) D3MapKey (D3Map d) (D3Map d)
foreign import d3mapEachFn    :: ∀ d eff. EffFn2 (d3::D3|eff) D3KVFn   (D3Map d) (D3Map d)
foreign import d3mapSetFn     :: ∀ d eff. EffFn2 (d3::D3|eff) D3MapKey    d      (D3Map d)

-- | Purescript constructor for D3Map
-- Lot of type unsafety here - we're giving the D3 map constructor a type c and claiming it
-- gives us back a Map of type d, assumes c is a valid homomorphous collection of d's
d3Map :: ∀ c d eff. c -> Eff (d3::D3|eff) (D3Map d)
d3Map c  = runEffFn1 d3MapFn c
-- | Also, this callback does potentially add real overhead since it's called
-- for all elements of the map but it's gotta be done this way if you're going
-- to pass in a Purescript lambda (maybe you could add a third variation that
-- takes an FFI function instead of passing Lambda)
d3MapF :: ∀ c d eff. c -> (d -> D3MapKey) -> Eff (d3::D3|eff) (D3Map d)
d3MapF c f = runEffFn2 d3MapFnFn c f

-- | Functions available on D3Maps, similar but not same to ES5 Maps
d3mapClear   :: ∀ d eff. (D3Map d)                  -> Eff (d3::D3|eff) (D3Map d)
d3mapClear = runEffFn1 d3mapClearFn

d3mapEach    :: ∀ d eff. D3KVFn -> (D3Map d)        -> Eff (d3::D3|eff) (D3Map d)
d3mapEach = runEffFn2 d3mapEachFn

d3mapEmpty   :: ∀ d eff. (D3Map d)                  -> Eff (d3::D3|eff) Boolean
d3mapEmpty = runEffFn1 d3mapEmptyFn

d3mapEntries :: ∀ d eff. (D3Map d)                  -> Eff (d3::D3|eff) (Array (D3KeyValue d))
d3mapEntries = runEffFn1 d3mapEntriesFn

d3mapGet     :: ∀ d eff. D3MapKey                   -> Eff (d3::D3|eff)  d
d3mapGet = runEffFn1 d3mapGetFn

d3mapHas     :: ∀ d eff. D3MapKey -> (D3Map d)      -> Eff (d3::D3|eff) Boolean
d3mapHas = runEffFn2 d3mapHasFn

d3mapKeys    :: ∀ d eff. (D3Map d)                  -> Eff (d3::D3|eff) (Array D3MapKey)
d3mapKeys = runEffFn1 d3mapKeysFn

d3mapRemove  :: ∀ d eff. D3MapKey -> (D3Map d)      -> Eff (d3::D3|eff) (D3Map d)
d3mapRemove = runEffFn2 d3mapRemoveFn

d3mapSet     :: ∀ d eff. D3MapKey -> d              -> Eff (d3::D3|eff) (D3Map d)
d3mapSet = runEffFn2 d3mapSetFn

d3mapSize    :: ∀ d eff. (D3Map d)                  -> Eff (d3::D3|eff) Number
d3mapSize = runEffFn1 d3mapSizeFn

d3mapValues  :: ∀ d eff. (D3Map d)                  -> Eff (d3::D3|eff) (Array d)
d3mapValues = runEffFn1 d3mapValuesFn
