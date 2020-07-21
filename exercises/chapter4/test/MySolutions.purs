module Test.MySolutions where

import Prelude
import Test.Examples (factors, allFiles)
import Data.Path
import Data.Foldable (foldl, foldr)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Int (rem, quot)
import Data.Array(cons, null, tail, head, last, filter, length, (..), concatMap)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Control.MonadZero (guard)

-- Note to reader: Add your solutions to this file

isEven 0 = true
isEven 1 = false
isEven x = not $ isEven (x - 1)

oneIfEven n = if isEven n then 1 else 0

countEven xs =
  if null xs
  then 0
  else x + (countEven $ fromMaybe [] $ tail xs)
    where
      x = maybe 0 oneIfEven $ head xs

squared :: Array Number -> Array Number
squared = map (\x -> x * x)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter ((<=) 0.0)

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite arr = ((<=) 0.0) <$?> arr

isPrime :: Int -> Boolean
isPrime = ((==) 1) <<< length <<< factors

cartesianProduct :: forall a. Array a -> Array a -> Array(Array a)
cartesianProduct arr1 arr2 = do
  a1 <- arr1
  a2 <- arr2
  pure [a1, a2]

triples :: Int -> Array (Array Int)
triples n = do
  i <- 1 .. n
  j <- i .. n
  k <- j .. n
  guard $ i * i + j * j == k * k
  pure [i, j, k]

factorizations :: Int -> Array Int
factorizations n = factorizations' 2 n []
  where
  factorizations' :: Int -> Int -> Array Int -> Array Int
  factorizations' _ 1 result = result

  factorizations' divisor dividend result =
    let
      remainder = rem dividend divisor
    in
      if remainder == 0 then
        factorizations' (divisor) (quot dividend divisor) (cons divisor result)
      else
        factorizations' (divisor + 1) dividend result


allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

fibTailRec :: Int -> Int
fibTailRec 0 = 1
fibTailRec 1 = 1
fibTailRec n = fib' n 0 0 1
  where
    fib' limit count n1 n2 =
      if limit == count then
        n1 + n2
      else
        fib' limit (count + 1) (n1 + n2) n1

reverse :: forall a. Array a -> Array a
reverse = foldl (\acc a -> [a] <> acc) []

onlyFiles :: Path -> Array Path
onlyFiles = allFiles >>> filter (isDirectory >>> not)

whereIs :: String -> Maybe String
whereIs fileName = head $ do
    path <- allFiles root
    child <- ls path
    guard $ eq fileName $ fromMaybe "" $ last $ split (Pattern "/") $ filename child
    pure $ filename path
