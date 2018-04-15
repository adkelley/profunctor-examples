module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (range)
import Data.Char (toUpper)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Int (odd)
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor, dimap, lmap, rmap)
import Data.String (length, singleton, toLower, uncons)
import Data.String.Yarn (unwords, words)

newtype Predicate a = Predicate (a -> Boolean)

instance contraPred :: Contravariant Predicate where
  cmap g (Predicate p) = Predicate (p <<< g)

-- Functors don't necessarily have to be containery types!!
-- Notice my f here is (a -> Boolean)
getPredicate :: ∀ a. Predicate a -> (a -> Boolean)
getPredicate (Predicate p) = liftA1 id p

veryOdd :: Predicate Int
veryOdd = cmap (_ `div` 2) (Predicate odd)

toUpper' :: String -> String
toUpper' s = case uncons s of
  Just r -> singleton $ toUpper $ r.head
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


main :: ∀ e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Fun w/ Profunctors!"
  log $ lmap f (\s -> "lmap f g = g . f = " <> show s) ["hello", "sailor"]
  log $ rmap (\s -> "rmap f g = f . g = " <> show s) g  ["HELLO", "SAILOR"]
  log $ dimap f (\s -> "dimap h f g = f . g . h = " <> show s) h ["hello", "sailor"]
  log $ "capitalize: " <> capitalize "the quick brown fox jUMPS OVER thE LaZY DOG"
  log $ "veryOdd: " <> (show $ getPredicate veryOdd <$> (range 0 11))
