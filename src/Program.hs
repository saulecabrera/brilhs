{-# LANGUAGE OverloadedStrings #-}
module Program
  ( Program(..)
  ) where

import           Data.Aeson
import           Fn         (Fn)

newtype Program = Program [Fn] deriving (Show, Eq)

instance FromJSON Program where
  parseJSON = withObject "program" $ \o ->
    Program <$> o .: "functions"

instance ToJSON Program where
  toJSON (Program fs) = object ["functions" .= fs]
