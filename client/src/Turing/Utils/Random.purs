module Turing.Utils.Random where

import Prelude

import Effect (Effect)
import Effect.Random (randomInt)
import Data.Unfoldable (replicateA)
import Data.String (length, codePointAt, fromCodePointArray, CodePoint, codePointFromChar)
import Data.Maybe (fromMaybe)

alphabet :: String
alphabet = "0123456789abcdefghijklmnopqrstuvwxyz"

maximumIndex :: Int
maximumIndex = length alphabet - 1

randomCodePoint :: Effect CodePoint
randomCodePoint = do
    index <- randomInt 0 maximumIndex
    pure $ fromMaybe (codePointFromChar '_') $ codePointAt index alphabet

randomString :: Int -> Effect String
randomString length = do
    codePoints <- replicateA length randomCodePoint
    pure $ fromCodePointArray codePoints
