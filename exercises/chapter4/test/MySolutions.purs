module Test.MySolutions where

import Prelude
import Data.Ord (abs)
import Data.Int (rem)
import Data.Foldable (foldl)
import Data.Array (head, tail, null, filter, (..))
import Data.Maybe (Maybe(..), fromMaybe)
import Test.Examples (factors)
import Control.Alternative (guard)
import Data.Path

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = not $ isEven $ abs n - 1

countEven :: Array Int -> Int
countEven [] = 0
countEven ns =
  if isEvenHead then 1 + tailEvens
  else tailEvens
  where
    isEvenHead = isEven $ fromMaybe 0 $ head ns
    tailEvens = countEven $ fromMaybe [] $ tail ns

squared :: Array Number -> Array Number
squared ns = (\n -> n * n) <$> ns

keepNonNegative :: Array Number -> Array Number
keepNonNegative ns = filter (_ >= 0.0) ns

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite ns = (_ >= 0.0) <$?> ns


isPrime :: Int -> Boolean
isPrime 1 = false
isPrime n = factors n == [[1,n]]

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct as bs = do
  a <- as
  b <- bs
  pure $ [a, b]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a*a + b*b == c*c
  pure $ [a, b, c]

primeFactors :: Int -> Array Int
primeFactors 1 = []
primeFactors n = [p1] <> primeFactors (n/p1)
  where p1 = fromMaybe 1 $ head $ do
          p <- 2 .. n
          guard $ n `rem` p == 0
          pure p

allTrue :: Array Boolean -> Boolean
allTrue = foldl (\r b -> r && b) true

fibTailRec :: Int -> Int
fibTailRec n = fibA n 1 0

fibA :: Int -> Int -> Int -> Int
fibA 0 _ _ = 0
fibA 1 fn _ = fn
fibA n1 fn fn1 = fibA (n1 - 1) (fn + fn1) fn

reverse :: forall a. Array a -> Array a
reverse as = foldl (\ras b -> [b] <> ras) [] as

onlyFiles :: Path -> Array Path
onlyFiles p = if isDirectory p then onlyFiles' else [p]
  where onlyFiles' = do
          child <- ls p
          onlyFiles child

whereIs :: Path -> String -> Maybe Path
whereIs p f = head $ locate p f

locate :: Path -> String -> Array Path
locate p f = do
  child <- ls p
  if filename child == filename p <> f
    then pure p
    else locate child f


largestSmallest :: Path -> Array Path
largestSmallest p = case largestSmallest' p of
  [a, b] -> if filename a == filename b then [a] else [a, b]
  other -> other


largestSmallest' :: Path -> Array Path
largestSmallest' p =
  if isDirectory p
  then
    foldl foo [] $ largestSmallest' <$> ls p
  else
    [p, p]
  where
    foo [] minmax = minmax
    foo [a,b] [c, d] =
      [ if size a > size c then c else a
      , if size b < size d then d else b
      ]
    foo _ _ = [] -- shouldn't happen
