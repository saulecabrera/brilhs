{-# LANGUAGE OverloadedStrings #-}
module Program
  ( Program(..)
  ) where

import Fn (Fn) 
import Data.Aeson

newtype Program = Program [Fn] deriving (Show, Eq)

instance FromJSON Program where
  parseJSON = withObject "program" $ \o ->
    Program <$> o .: "functions"

instance ToJSON Program where
  toJSON (Program fs) = object ["functions" .= fs]
