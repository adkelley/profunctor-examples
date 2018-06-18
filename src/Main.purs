module Main where

import Prelude

import Data.Array (range)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Int (odd)
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor, dimap, lcmap, rmap)
import Data.String (Pattern(..), joinWith, length, singleton, split, toLower, toUpper, uncons)
import Effect (Effect)
import Effect.Console (log)

newtype Predicate a = Predicate (a -> Boolean)

instance contraPred :: Contravariant Predicate where
  cmap k (Predicate p) = Predicate (p <<< k)

words :: String → Array String
words = split (Pattern " ")

unwords :: Array String → String
unwords = joinWith " "

-- Functors don't necessarily have to be containery types!!
-- Notice my f here is (a -> Boolean)
getPredicate :: ∀ a. Predicate a -> (a -> Boolean)
getPredicate (Predicate p) = liftA1 identity p

veryOdd :: Predicate Int
veryOdd = cmap (_ `div` 2) (Predicate odd)

toUpper' :: String -> String
toUpper' s = case uncons s of
  Just r -> toUpper $ singleton r.head
  Nothing -> ""

tail' :: String -> String
tail' s = case uncons s of
  Just r -> r.tail
  Nothing -> ""

capWord :: String -> String
capWord s = (toUpper' s) <> (toLower $ tail' s)

onWords :: ∀ p. Profunctor p => p (Array String) (Array String) -> p String String
onWords = dimap words unwords

capitalize :: String -> String
capitalize = onWords (map capWord)

f :: Array String -> Array Int
f = map length

g :: Array String -> Array String
g = map toLower

h :: Array Int -> Array Int
h = map (\x -> 1 + x)


main :: Effect Unit
main = do
  log "Fun w/ Profunctors!"
  log $ lcmap f (\s -> "lcmap f g = g . f = " <> show s) ["hello", "sailor"]
  log $ rmap (\s -> "rmap f g = f . g = " <> show s) g  ["HELLO", "SAILOR"]
  log $ dimap f (\s -> "dimap h f g = f . g . h = " <> show s) h ["hello", "sailor"]
  log $ "capitalize: " <> capitalize "the quick brown fox jUMPS OVER thE LaZY DOG"
  log $ "veryOdd: " <> (show $ getPredicate veryOdd <$> (range 0 11))
