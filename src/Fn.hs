{-# LANGUAGE OverloadedStrings #-}
module Fn
  ( Fn(..)
  , void
  )
  where

import           Data.Aeson
import           Id         (Dest, Ident, OptionalType)
import           Instr      (Instr)

data Fn = Fn Ident [Dest] OptionalType [Instr] deriving (Show, Eq)

instance FromJSON Fn where
  parseJSON = withObject "function" $ \o ->
    Fn <$> o .: "name"
       <*> (o .:? "args" >>= maybe (pure []) pure)
       <*> o .:? "type"
       <*> o .: "instrs"

instance ToJSON Fn where
  toJSON (Fn name args Nothing instrs) =
    object [ "name" .= name
           , "args" .= args
           , "instrs" .= instrs
           ]

  toJSON (Fn name args (Just ty) instrs) =
    object [ "name" .= name
           , "args" .= args
           , "instrs" .= instrs
           , "type" .= ty
           ]

void name args = Fn name args Nothing


