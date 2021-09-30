{-# LANGUAGE OverloadedStrings #-}
module Program
  ( Program(..)
  ) where

import Fn (Fn) 
import Data.Aeson

newtype Program = Program [Fn] deriving (Show)

instance FromJSON Program where
  parseJSON = withObject "program" $ \o ->
    Program <$> o .: "functions"
