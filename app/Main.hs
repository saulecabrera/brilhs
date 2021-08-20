{-# LANGUAGE OverloadedStrings #-}
module Main where
import Bril
import System.Environment
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BLU


main :: IO ()
main = do
  args <- getArgs
  contents <- BLU.readFile $ Prelude.head args
  case (eitherDecode contents :: Either String Program) of
    Right program ->   print $ formBlocks program
    Left reason -> print reason

