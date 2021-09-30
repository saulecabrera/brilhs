{-# LANGUAGE OverloadedStrings #-}
module Fn 
  ( Fn(..)
  )
  where 

import Instr (Instr)
import Id (Ident, Dest, OptionalType)
import Data.Aeson

data Fn = Fn Ident [Dest] OptionalType [Instr] deriving (Show)

instance FromJSON Fn where
  parseJSON = withObject "function" $ \o ->
    Fn <$> o .: "name"
       <*> (o .:? "args" >>= maybe (pure []) pure)
       <*> o .:? "type"
       <*> o .: "instrs"
