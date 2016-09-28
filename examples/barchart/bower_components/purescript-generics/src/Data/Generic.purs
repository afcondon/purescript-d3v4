module Data.Generic
  ( class Generic
  , toSpine
  , toSignature
  , fromSpine
  , GenericSpine(..)
  , GenericSignature(..)
  , DataConstructor
  , showDataConstructor
  , showSignature
  , isValidSpine
  , gShow
  , gEq
  , gCompare
  ) where

import Prelude

import Data.Array (null, length, sortBy, zipWith)
import Data.Either (Either(..))
import Data.Foldable (all, and, find, fold, intercalate)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))

-- | The Generic typeclass provides methods for sending data to/from spine
-- | representations, as well as querying about the signatures of spine
-- | representations.
-- |
-- | For standard data structures, you can simply write
-- | `derive instance genericFoo :: Generic Foo` in the module they are
-- | declared, and the instance methods will be filled in for you.
class Generic a where
  toSpine :: a -> GenericSpine
  toSignature :: Proxy a -> GenericSignature
  fromSpine :: GenericSpine -> Maybe a

instance genericNumber :: Generic Number where
  toSpine = SNumber
  toSignature _ = SigNumber
  fromSpine (SNumber n) = Just n
  fromSpine _ = Nothing

instance genericInt :: Generic Int where
  toSpine = SInt
  toSignature _ = SigInt
  fromSpine (SInt n) = Just n
  fromSpine _ = Nothing

instance genericString :: Generic String where
  toSpine = SString
  toSignature _ = SigString
  fromSpine (SString s) = Just s
  fromSpine _ = Nothing

instance genericChar :: Generic Char where
  toSpine = SChar
  toSignature _ = SigChar
  fromSpine (SChar s) = Just s
  fromSpine _ = Nothing

instance genericBool :: Generic Boolean where
  toSpine = SBoolean
  toSignature _ = SigBoolean
  fromSpine (SBoolean b) = Just b
  fromSpine _ = Nothing

instance genericArray :: Generic a => Generic (Array a) where
  toSpine = SArray <<< map (\x _ -> toSpine x)
  toSignature x = SigArray (\_ -> toSignature (lowerProxy x))
    where
    lowerProxy :: Proxy (Array a) -> Proxy a
    lowerProxy _ = Proxy
  fromSpine (SArray x) = traverse (fromSpine <<< force) x
  fromSpine _ = Nothing

instance genericUnit :: Generic Unit where
  toSpine _ = SUnit
  toSignature _ = SigUnit
  fromSpine SUnit = Just unit
  fromSpine _ = Nothing

instance genericTuple :: (Generic a, Generic b) => Generic (Tuple a b) where
  toSpine (Tuple x y) =
    SProd "Data.Tuple.Tuple" [\_ -> toSpine x, \_ -> toSpine y]
  toSignature x =
    SigProd
      "Data.Tuple.Tuple"
      [ { sigConstructor: "Data.Tuple.Tuple"
        , sigValues:
            [ \_ -> toSignature (fstProxy x)
            , \_ -> toSignature (sndProxy x)
            ]
        }
      ]
    where
    fstProxy :: Proxy (Tuple a b) -> Proxy a
    fstProxy _ = Proxy
    sndProxy :: Proxy (Tuple a b) -> Proxy b
    sndProxy _ = Proxy
  fromSpine (SProd "Data.Tuple.Tuple" [x, y]) =
    Tuple <$> fromSpine (force x) <*> fromSpine (force y)
  fromSpine _ = Nothing

instance genericMaybe :: Generic a => Generic (Maybe a) where
  toSpine (Just x) = SProd "Data.Maybe.Just" [\_ -> toSpine x]
  toSpine Nothing = SProd "Data.Maybe.Nothing" []
  toSignature x =
    SigProd
      "Data.Maybe.Maybe"
      [ { sigConstructor: "Data.Maybe.Just"
        , sigValues: [\_ -> toSignature (mbProxy x)]
        }
      , { sigConstructor: "Data.Maybe.Nothing"
        , sigValues: []
        }
      ]
    where
    mbProxy :: Proxy (Maybe a) -> Proxy a
    mbProxy _ = Proxy
  fromSpine (SProd "Data.Maybe.Just" [x]) = Just <$> fromSpine (force x)
  fromSpine (SProd "Data.Maybe.Nothing" []) = pure Nothing
  fromSpine _ = Nothing

instance genericEither :: (Generic a, Generic b) => Generic (Either a b) where
  toSpine (Left x) = SProd "Data.Either.Left" [\_ -> toSpine x]
  toSpine (Right x) = SProd "Data.Either.Right" [\_ -> toSpine x]
  toSignature x =
    SigProd
      "Data.Either.Either"
      [ { sigConstructor: "Data.Either.Left"
        , sigValues: [\_ -> toSignature (lproxy x)]
        }
      , { sigConstructor: "Data.Either.Right"
        , sigValues: [\_ -> toSignature (rproxy x)]
        }
      ]
    where
    lproxy :: Proxy (Either a b) -> Proxy a
    lproxy _ = Proxy
    rproxy :: Proxy (Either a b) -> Proxy b
    rproxy _ = Proxy
  fromSpine (SProd "Data.Either.Left" [x]) = Left <$> fromSpine (force x)
  fromSpine (SProd "Data.Either.Right" [x]) = Right <$> fromSpine (force x)
  fromSpine _ = Nothing

instance genericOrdering :: Generic Ordering where
  toSpine = case _ of
    LT -> SProd "Data.Ordering.LT" []
    EQ -> SProd "Data.Ordering.EQ" []
    GT -> SProd "Data.Ordering.GT" []
  toSignature _ =
    SigProd
      "Data.Ordering.Ordering"
      [ { sigConstructor: "Data.Ordering.LT", sigValues: [] }
      , { sigConstructor: "Data.Ordering.EQ", sigValues: [] }
      , { sigConstructor: "Data.Ordering.GT", sigValues: [] }
      ]
  fromSpine = case _ of
    SProd "Data.Ordering.LT" [] -> Just LT
    SProd "Data.Ordering.EQ" [] -> Just EQ
    SProd "Data.Ordering.GT" [] -> Just GT
    _ -> Nothing

-- | A GenericSpine is a universal representation of an arbitrary data
-- | structure (that does not contain function arrows).
data GenericSpine
  = SProd String (Array (Unit -> GenericSpine))
  | SRecord (Array { recLabel :: String, recValue :: Unit -> GenericSpine })
  | SNumber Number
  | SBoolean Boolean
  | SInt Int
  | SString String
  | SChar Char
  | SArray (Array (Unit -> GenericSpine))
  | SUnit

instance eqGenericSpine :: Eq GenericSpine where
  eq (SProd s1 arr1) (SProd s2 arr2) =
    s1 == s2 && length arr1 == length arr2 && zipAll eqThunk arr1 arr2
  eq (SRecord arr1) (SRecord arr2) = eqRecordSigs arr1 arr2
  eq (SNumber x) (SNumber y) = x == y
  eq (SBoolean x) (SBoolean y) = x == y
  eq (SInt x) (SInt y) = x == y
  eq (SString x) (SString y) = x == y
  eq (SChar x) (SChar y) = x == y
  eq (SArray xs) (SArray ys) = length xs == length ys && zipAll eqThunk xs ys
  eq SUnit SUnit = true
  eq _ _ = false

instance ordGenericSpine :: Ord GenericSpine where
  compare (SProd s1 arr1) (SProd s2 arr2) =
    case compare s1 s2 of
      EQ -> compare 0 $ zipCompare compareThunk arr1 arr2
      c1 -> c1
  compare (SProd _ _) _ = LT
  compare _ (SProd _ _) = GT
  compare (SRecord xs) (SRecord ys) =
    compare 0 $ zipCompare go xs ys
    where
    go x y = case compare x.recLabel y.recLabel of
      EQ -> orderingToInt $ compare (force x.recValue) (force y.recValue)
      c -> orderingToInt c
  compare (SRecord _) _ = LT
  compare _ (SRecord _) = GT
  compare (SInt x) (SInt y) = compare x y
  compare (SInt _) _ = LT
  compare _ (SInt _) = GT
  compare (SBoolean x) (SBoolean y) = compare x y
  compare (SBoolean _) _ = LT
  compare _ (SBoolean _) = GT
  compare (SNumber x) (SNumber y) = compare x y
  compare (SNumber _) _ = LT
  compare _ (SNumber _) = GT
  compare (SString x) (SString y) = compare x y
  compare (SString _) _ = LT
  compare _ (SString _) = GT
  compare (SChar x) (SChar y) = compare x y
  compare (SChar _) _ = LT
  compare _ (SChar _) = GT
  compare (SArray xs) (SArray ys) = compare 0 $ zipCompare compareThunk xs ys
  compare (SArray _) _ = LT
  compare _ (SArray _) = GT
  compare SUnit SUnit = EQ

-- | A GenericSignature is a universal representation of the structure of an
-- | arbitrary data structure (that does not contain function arrows).
data GenericSignature
  = SigProd String (Array DataConstructor)
  | SigRecord (Array { recLabel :: String, recValue :: Unit -> GenericSignature })
  | SigNumber
  | SigBoolean
  | SigInt
  | SigString
  | SigChar
  | SigArray (Unit -> GenericSignature)
  | SigUnit

instance eqGenericSignature :: Eq GenericSignature where
  eq (SigProd s1 arr1) (SigProd s2 arr2) =
    s1 == s2 && length arr1 == length arr2 && zipAll eqDataConstructor arr1 arr2
  eq (SigRecord arr1) (SigRecord arr2) = eqRecordSigs arr1 arr2
  eq SigNumber SigNumber = true
  eq SigBoolean SigBoolean = true
  eq SigInt SigInt = true
  eq SigString SigString = true
  eq SigChar SigChar = true
  eq (SigArray t1) (SigArray t2) = eqThunk t1 t2
  eq SigUnit SigUnit = true
  eq _ _ = false

instance showGenericSignature :: Show GenericSignature where
  show = showSignature

-- | Identifies a data constructor.
type DataConstructor =
  { sigConstructor :: String
  , sigValues :: Array (Unit -> GenericSignature)
  }

eqDataConstructor :: DataConstructor -> DataConstructor -> Boolean
eqDataConstructor p1 p2
  = p1.sigConstructor == p2.sigConstructor
  && zipAll eqThunk p1.sigValues p2.sigValues

showDataConstructor :: DataConstructor -> String
showDataConstructor dc =
  "{ sigConstructor: " <> show dc.sigConstructor <>
  ", sigValues: " <> showArray (showSignature <<< force) dc.sigValues <>
  "}"

showSignature :: GenericSignature -> String
showSignature sig =
  fold $ case sig of
    SigProd tyName ctors ->
      ["SigProd ", show tyName, " ", showArray showDataConstructor ctors]
    SigRecord labels -> ["SigRecord ", showArray showLabel labels]
    SigNumber -> ["SigNumber"]
    SigBoolean -> ["SigBoolean"]
    SigInt -> ["SigInt"]
    SigString -> ["SigString"]
    SigChar -> ["SigChar"]
    SigArray sig' -> ["SigArray ", paren (force sig')]
    SigUnit -> ["SigUnit"]

  where
  paren s
    | needsParen s = "(" <> showSignature s <> ")"
    | otherwise = showSignature s

  needsParen s = case s of
    SigProd _ _ -> true
    SigRecord _ -> true
    SigNumber -> false
    SigBoolean -> false
    SigInt -> false
    SigString -> false
    SigChar -> false
    SigArray _ -> true
    SigUnit -> false

-- We use this instead of the default Show Array instance to avoid escaping
-- strings twice.
showArray :: forall a. (a -> String) -> Array a -> String
showArray f xs = "[ " <> intercalate ", " (map f xs) <> " ]"

showLabel
  :: { recLabel :: String, recValue :: Unit -> GenericSignature }
  -> String
showLabel l =
  "{ recLabel: " <> show l.recLabel <>
  ", recValue: " <> showSignature (force l.recValue) <>
  " }"

-- | Checks that the spine follows the structure defined by the signature
isValidSpine :: GenericSignature -> GenericSpine -> Boolean
isValidSpine SigBoolean (SBoolean _) = true
isValidSpine SigNumber (SNumber _) = true
isValidSpine SigInt (SInt _) = true
isValidSpine SigString (SString _) = true
isValidSpine SigChar (SChar _) = true
isValidSpine (SigArray sig) (SArray spines) =
  all (isValidSpine (force sig) <<< force) spines
isValidSpine (SigProd _ alts) (SProd tag values) =
  case find (\alt -> alt.sigConstructor == tag) alts of
    Nothing -> false
    Just { sigValues } ->
      and $ zipWith
        (\sig spine -> isValidSpine (force sig) (force spine))
        sigValues
        values
isValidSpine (SigRecord fieldSigs) (SRecord fieldVals) =
  and $ zipWith
    (\sig val -> isValidSpine (force sig.recValue) (force val.recValue))
    (sortBy (\a b -> compare a.recLabel b.recLabel) fieldSigs)
    (sortBy (\a b -> compare a.recLabel b.recLabel) fieldVals)
isValidSpine SigUnit SUnit = true
isValidSpine _ _ = false

-- ## Generic Functions

-- | This function can be used as the default instance for Show for any
-- | instance of Generic
gShow :: forall a. Generic a => a -> String
gShow = genericShowPrec 0 <<< toSpine

genericShowPrec :: Int -> GenericSpine -> String
genericShowPrec d (SProd s arr)
  | null arr = s
  | otherwise =
      showParen (d > 10) $
        s <> " " <> joinWith " " (map (\x -> genericShowPrec 11 (force x)) arr)
      where
      showParen false x = x
      showParen true  x = "(" <> x <> ")"
genericShowPrec _ (SRecord xs) =
  "{" <> joinWith ", " (map showLabelPart xs) <> "}"
  where
  showLabelPart x = x.recLabel <> ": " <> genericShowPrec 0 (force x.recValue)
genericShowPrec _ (SBoolean x) = show x
genericShowPrec _ (SInt x) = show x
genericShowPrec _ (SNumber x) = show x
genericShowPrec _ (SString x) = show x
genericShowPrec _ (SChar x) = show x
genericShowPrec _ (SArray xs) =
  "[" <> joinWith ", "  (map (\x -> genericShowPrec 0 (force x)) xs) <> "]"
genericShowPrec _ SUnit = "unit"

-- | This function can be used as an implementation of the `eq` function of `Eq`
-- | for any type with a `Generic` instance.
-- |
-- | **Note**: It is preferrable to use `derive instance` for `Eq` instances
-- | rather than relying on `gEq`, where possible.
gEq :: forall a. Generic a => a -> a -> Boolean
gEq x y = toSpine x == toSpine y

-- | This function can be used as an implementation of the `compare` function
-- | of `Ord` for any type with a `Generic` instance.
-- |
-- | **Note**: It is preferrable to use `derive instance` for `Ord` instances
-- | rather than relying on `gCompare`, where possible.
gCompare :: forall a. Generic a => a -> a -> Ordering
gCompare x y = compare (toSpine x) (toSpine y)

-- ## Internal functions

foreign import zipAll
  :: forall a b
   . (a -> b -> Boolean)
  -> Array a
  -> Array b
  -> Boolean

foreign import zipCompare
  :: forall a b
   . (a -> b -> Int)
  -> Array a
  -> Array b
  -> Int

force :: forall a. (Unit -> a) -> a
force f = f unit

compareThunk :: forall a. Ord a => (Unit -> a) -> (Unit -> a) -> Int
compareThunk x y = orderingToInt $ compare (force x) (force y)

eqThunk :: forall a. Eq a => (Unit -> a) -> (Unit -> a) -> Boolean
eqThunk x y = force x == force y

eqRecordSigs
  :: forall a
   . Eq a
  => Array {recLabel :: String, recValue :: Unit -> a}
  -> Array {recLabel :: String, recValue :: Unit -> a}
  -> Boolean
eqRecordSigs arr1 arr2 =
  length arr1 == length arr2 && zipAll doCmp sorted1 sorted2
  where
  labelCompare r1 r2 = compare r1.recLabel r2.recLabel
  sorted1 = sortBy labelCompare arr1
  sorted2 = sortBy labelCompare arr2
  doCmp x y = x.recLabel == y.recLabel && force x.recValue == force y.recValue

orderingToInt :: Ordering -> Int
orderingToInt = case _ of
  EQ -> 0
  LT -> 1
  GT -> -1
