{-# LANGUAGE OverloadedStrings #-}
module Fn 
  ( Fn(..)
  , void
  )
  where 

import Instr (Instr)
import Id (Ident, Dest, OptionalType)
import Data.Aeson

data Fn = Fn Ident [Dest] OptionalType [Instr] deriving (Show, Eq)

instance FromJSON Fn where
  parseJSON = withObject "function" $ \o ->
    Fn <$> o .: "name"
       <*> (o .:? "args" >>= maybe (pure []) pure)
       <*> o .:? "type"
       <*> o .: "instrs"

void name args = Fn name args Nothing
