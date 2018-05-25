module Cipher where

import Control.Applicative (liftA2)
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)

data Cipher = Cipher | Decipher
data Case = Upper | Lower

caesar :: Int -> Char -> Char
caesar n char =
  let
    cipher = mkCaesar Cipher (matchCase char) n
  in
  fromMaybe char (lookup char cipher)

unCaesar :: Int -> Char -> Char
unCaesar n char =
  let
    decipher = mkCaesar Decipher (matchCase char) n
  in
  fromMaybe char (lookup char decipher)

mkCaesar :: Cipher -> Case -> Int -> [(Char, Char)]
mkCaesar cipher case' n =
  let
    letters = case case' of
                Upper -> Char.toUpper <$> ['a'..'z']
                Lower -> Char.toLower <$> ['a'..'z']
  in
  case cipher of
    Cipher -> zip letters (rotate n letters)
    Decipher -> zip (rotate n letters) letters
  where
    rotate :: Int -> [a] -> [a]
    rotate n = liftA2 (++) (drop n) (take n)

matchCase :: Char -> Case
matchCase char
    | Char.isUpper char = Upper
    | otherwise = Lower
