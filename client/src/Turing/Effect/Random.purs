module Turing.Effect.Random (randomString, randomId) where

import Prelude
import Effect (Effect)
import Effect.Random (randomInt)
import Data.String (length, codePointAt, fromCodePointArray)
import Data.Maybe (fromMaybe)
import Data.String.CodePoints (CodePoint, codePointFromChar)
import Data.Unfoldable (replicateA)

alphabet :: String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz012345689"

randomCodePoint :: Effect CodePoint
randomCodePoint = do
  index <- randomInt 0 (length alphabet - 1)
  pure $ fromMaybe (codePointFromChar '_') $ codePointAt index alphabet

randomString :: Int -> Effect String
randomString length = do
  codePoints <- replicateA length randomCodePoint
  pure $ fromCodePointArray codePoints

randomId :: Effect String
randomId = randomString 7
