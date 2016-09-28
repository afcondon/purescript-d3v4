module Data.Enum
  ( class Enum, succ, pred
  , defaultSucc
  , defaultPred
  , enumFromTo
  , enumFromThenTo
  , upFrom
  , downFrom
  , Cardinality(..), runCardinality
  , class BoundedEnum, cardinality, toEnum, fromEnum, toEnumWithDefaults
  , defaultCardinality
  , defaultToEnum
  , defaultFromEnum
  ) where

import Prelude

import Control.MonadPlus (guard)

import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, fromJust)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)

import Partial.Unsafe (unsafePartial)

newtype Cardinality a = Cardinality Int

runCardinality :: forall a. Cardinality a -> Int
runCardinality (Cardinality a) = a

-- | Type class for enumerations.
-- |
-- | Laws:
-- | - `succ a > pred a`
-- | - `pred a < succ a`
-- | - `pred >=> succ >=> pred = pred`
-- | - `succ >=> pred >=> succ = succ`
class Ord a <= Enum a where
  succ :: a -> Maybe a
  pred :: a -> Maybe a

instance enumBoolean :: Enum Boolean where
  succ false = Just true
  succ _ = Nothing
  pred true = Just false
  pred _= Nothing

instance enumInt :: Enum Int where
  succ n = if n < top then Just (n + 1) else Nothing
  pred n = if n > bottom then Just (n - 1) else Nothing

instance enumChar :: Enum Char where
  succ = defaultSucc charToEnum toCharCode
  pred = defaultPred charToEnum toCharCode

charToEnum :: Int -> Maybe Char
charToEnum n | n >= bottom && n <= top = Just $ fromCharCode n
charToEnum _ = Nothing

instance enumUnit :: Enum Unit where
  succ = const Nothing
  pred = const Nothing

instance enumOrdering :: Enum Ordering where
  succ LT = Just EQ
  succ EQ = Just GT
  succ GT = Nothing
  pred LT = Nothing
  pred EQ = Just LT
  pred GT = Just EQ

instance enumMaybe :: BoundedEnum a => Enum (Maybe a) where
  succ Nothing = Just $ Just bottom
  succ (Just a) = Just <$> succ a
  pred Nothing = Nothing
  pred (Just a) = Just $ pred a

instance enumEither :: (BoundedEnum a, BoundedEnum b) => Enum (Either a b) where
  succ (Left a) = maybe (Just $ Right bottom) (Just <<< Left) (succ a)
  succ (Right b) = maybe (Nothing) (Just <<< Right) (succ b)
  pred (Left a) = maybe (Nothing) (Just <<< Left) (pred a)
  pred (Right b) = maybe (Just $ Left top) (Just <<< Right) (pred b)

instance enumTuple :: (Enum a, BoundedEnum b) => Enum (Tuple a b) where
  succ (Tuple a b) = maybe (flip Tuple bottom <$> succ a) (Just <<< Tuple a) (succ b)
  pred (Tuple a b) = maybe (flip Tuple top <$> pred a) (Just <<< Tuple a) (pred b)

-- | ```defaultSucc toEnum fromEnum = succ```
defaultSucc :: forall a. (Int -> Maybe a) -> (a -> Int) -> a -> Maybe a
defaultSucc toEnum' fromEnum' a = toEnum' (fromEnum' a + 1)

-- | ```defaultPred toEnum fromEnum = pred```
defaultPred :: forall a. (Int -> Maybe a) -> (a -> Int) -> a -> Maybe a
defaultPred toEnum' fromEnum' a = toEnum' (fromEnum' a - 1)

-- | Returns a successive sequence of elements from the lower bound to
-- | the upper bound (inclusive).
enumFromTo :: forall a u. (Enum a, Unfoldable u) => a -> a -> u a
enumFromTo from to = unfoldr go (Just from)
  where
    go mx = do
      x <- mx
      guard (x <= to)
      pure $ Tuple x (succ x)

-- | `[a,b..c]`
enumFromThenTo :: forall a. BoundedEnum a => a -> a -> a -> Array a
enumFromThenTo = unsafePartial \a b c ->
  let a' = fromEnum a
      b' = fromEnum b
      c' = fromEnum c
  in (toEnum >>> fromJust) <$> intStepFromTo (b' - a') a' c'

-- | Property: ```forall e in intStepFromTo step a b: a <= e <= b```
intStepFromTo :: Int -> Int -> Int -> Array Int
intStepFromTo step from to =
  unfoldr (\e ->
            if e <= to
            then Just $ Tuple e (e + step)  -- Output the value e, set the next state to (e + step)
            else Nothing                    -- End of the collection.
          ) from

diag :: forall a. a -> Tuple a a
diag a = Tuple a a

upFrom :: forall a u. (Enum a, Unfoldable u) => a -> u a
upFrom = unfoldr (map diag <<< succ)

downFrom :: forall a u. (Enum a, Unfoldable u) => a -> u a
downFrom = unfoldr (map diag <<< pred)

-- | Type class for finite enumerations.
-- |
-- | This should not be considered a part of a numeric hierarchy, as in Haskell.
-- | Rather, this is a type class for small, ordered sum types with
-- | statically-determined cardinality and the ability to easily compute
-- | successor and predecessor elements, e.g. `DayOfWeek`.
-- |
-- | Laws:
-- |
-- | - ```succ bottom >>= succ >>= succ ... succ [cardinality - 1 times] == top```
-- | - ```pred top    >>= pred >>= pred ... pred [cardinality - 1 times] == bottom```
-- | - ```forall a > bottom: pred a >>= succ == Just a```
-- | - ```forall a < top:  succ a >>= pred == Just a```
-- | - ```forall a > bottom: fromEnum <$> pred a = Just (fromEnum a - 1)```
-- | - ```forall a < top:  fromEnum <$> succ a = Just (fromEnum a + 1)```
-- | - ```e1 `compare` e2 == fromEnum e1 `compare` fromEnum e2```
-- | - ```toEnum (fromEnum a) = Just a```
class (Bounded a, Enum a) <= BoundedEnum a where
  cardinality :: Cardinality a
  toEnum :: Int -> Maybe a
  fromEnum :: a -> Int

instance boundedEnumBoolean :: BoundedEnum Boolean where
  cardinality = Cardinality 2
  toEnum 0 = Just false
  toEnum 1 = Just true
  toEnum _ = Nothing
  fromEnum false = 0
  fromEnum true = 1

instance boundedEnumInt :: BoundedEnum Int where
  cardinality = Cardinality (top - bottom)
  toEnum = Just
  fromEnum = id

instance boundedEnumChar :: BoundedEnum Char where
  cardinality = Cardinality (toCharCode top - toCharCode bottom)
  toEnum = charToEnum
  fromEnum = toCharCode

instance boundedEnumUnit :: BoundedEnum Unit where
  cardinality = Cardinality 1
  toEnum 0 = Just unit
  toEnum _ = Nothing
  fromEnum = const 0

instance boundedEnumOrdering :: BoundedEnum Ordering where
  cardinality = Cardinality 3
  toEnum 0 = Just LT
  toEnum 1 = Just EQ
  toEnum 2 = Just GT
  toEnum _ = Nothing
  fromEnum LT = 0
  fromEnum EQ = 1
  fromEnum GT = 2

instance boundedEnumMaybe :: BoundedEnum a => BoundedEnum (Maybe a) where
  cardinality = Cardinality $ runCardinality (cardinality :: Cardinality a) + 1
  toEnum = to cardinality
    where
    to :: Cardinality a -> Int -> Maybe (Maybe a)
    to _ 0 = Nothing
    to (Cardinality ca) n | n <= ca = Just $ toEnum (n - 1)
    to _ _ = Nothing
  fromEnum Nothing = 0
  fromEnum (Just e) = fromEnum e + 1

instance boundedEnumEither :: (BoundedEnum a, BoundedEnum b) => BoundedEnum (Either a b) where
  cardinality =
    Cardinality
      $ runCardinality (cardinality :: Cardinality a)
      + runCardinality (cardinality :: Cardinality b)
  toEnum = to cardinality cardinality
    where
    to :: Cardinality a -> Cardinality (Either a b) -> Int -> Maybe (Either a b)
    to (Cardinality ca) (Cardinality cab) n
      | n >= 0 && n < ca = Left <$> toEnum n
      | n >= ca && n < cab = Right <$> toEnum (n - ca)
      | otherwise = Nothing
  fromEnum (Left a) = fromEnum a
  fromEnum (Right b) = fromEnum b + runCardinality (cardinality :: Cardinality a)

instance boundedEnumTuple :: (BoundedEnum a, BoundedEnum b) => BoundedEnum (Tuple a b) where
  cardinality =
    Cardinality
      $ runCardinality (cardinality :: Cardinality a)
      * runCardinality (cardinality :: Cardinality b)
  toEnum = to cardinality
    where
    to :: Cardinality b -> Int -> Maybe (Tuple a b)
    to (Cardinality cb) n = Tuple <$> toEnum (n / cb) <*> toEnum (n `mod` cb)
  fromEnum = from cardinality
    where
    from :: Cardinality b -> Tuple a b -> Int
    from (Cardinality cb) (Tuple a b) = fromEnum a * cb + fromEnum b

-- | Runs in `O(n)` where `n` is `fromEnum top`
defaultCardinality :: forall a. (Bounded a, Enum a) => Cardinality a
defaultCardinality = Cardinality $ defaultCardinality' 1 (bottom :: a) where
  defaultCardinality' i = maybe i (defaultCardinality' $ i + 1) <<< succ

  -- | Runs in `O(n)` where `n` is `fromEnum a`
defaultToEnum :: forall a. (Bounded a, Enum a) => Int -> Maybe a
defaultToEnum n
  | n < 0 = Nothing
  | n == 0 = Just bottom
  | otherwise = defaultToEnum (n - 1) >>= succ

  -- | Runs in `O(n)` where `n` is `fromEnum a`
defaultFromEnum :: forall a. Enum a => a -> Int
defaultFromEnum = maybe 0 (\prd -> defaultFromEnum prd + 1) <<< pred

-- | Like `toEnum` but returns the first argument if `x` is less than
-- | `fromEnum bottom` and the second argument if `x` is greater than
-- | `fromEnum top`.
-- |
-- | ``` purescript
-- | toEnumWithDefaults False True (-1) -- False
-- | toEnumWithDefaults False True 0    -- False
-- | toEnumWithDefaults False True 1    -- True
-- | toEnumWithDefaults False True 2    -- True
-- | ```
toEnumWithDefaults :: forall a. BoundedEnum a => a -> a -> Int -> a
toEnumWithDefaults b t x = case toEnum x of
  Just enum -> enum
  Nothing -> if x < fromEnum (bottom :: a) then b else t
