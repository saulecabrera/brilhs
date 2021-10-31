{-# LANGUAGE OverloadedStrings #-}
module Instr
  ( Instr(..)
  , BinaryOperation(..)
  , UnaryOperation(..)
  , ret
  , terminator
  )
  where 

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Id (Literal, Arg, Dest(..), optionalDest, Ident)
import Data.Foldable as F
import Control.Applicative ((<|>))

data Instr = Label Text
           | Const Dest Literal
           | Binary Dest BinaryOperation Arg Arg
           | Unary Dest UnaryOperation Arg
           | Jmp Text
           | Br Arg Text Text
           | Ret (Maybe Arg)
           | Call (Maybe Dest) Ident [Arg]
           | Store Arg Arg
           | Free Arg
           | Speculate
           | Commit
           | Guard Arg Text
           | Phi Dest [Text] [Arg]
           | Print [Arg]
           | Nop
           deriving (Show, Eq)

data BinaryOperation = Add
                     | FAdd
                     | PtrAdd
                     | Mul
                     | FMul
                     | Sub
                     | FSub
                     | Div
                     | FDiv
                     | Eq
                     | FEq
                     | Lt
                     | FLt
                     | Gt
                     | FGt
                     | Le
                     | FLe
                     | Ge
                     | FGe
                     | And
                     | Or
                     deriving (Show, Eq)

data UnaryOperation = Not
                    | Id
                    | Load
                    | Alloc
                    deriving (Show, Eq)

ret :: Maybe [Maybe Arg] -> Instr
ret Nothing    = Ret Nothing
ret (Just [a]) = Ret a
ret (Just _)  = Ret Nothing

terminator :: Instr -> Bool
terminator instr =
  case instr of
    Jmp _ -> True
    Ret _ -> True
    Br {} -> True
    _     -> False

instance FromJSON Instr where
  parseJSON = withObject "instr" $ \o -> F.asum [
                                                Label <$> o .: "label"
                                              , parseOp o
                                              ]

parseOp :: Object -> Parser Instr
parseOp o = do
  op <- o .: "op" :: Parser Text
  case op of
    "const" ->
      Const <$> (Dest <$> o .: "type" <*> o .: "dest") <*> o .: "value"
    "print" ->
      Print <$> o .: "args"
    "nop" ->
      return Nop
    "commit" ->
      return Commit
    "speculate" ->
      return Speculate
    "free" ->
      Free . Prelude.head <$> (o .: "args")
    "store" ->
      Store . Prelude.head <$> (o .: "args") <*> ((!! 1) <$> (o .: "args"))
    "guard" ->
      Guard . Prelude.head <$> (o .: "args") <*> (Prelude.head <$> (o .: "labels"))
    "phi" ->
      Phi <$> (Dest <$> o .: "type" <*> o .: "dest") <*> o .: "labels" <*> o .: "args"
    "call" ->
      Call <$> (optionalDest <$> o .:? "type" <*> o .:? "dest") <*> (Prelude.head <$> o .: "funcs") <*> (o .:? "args" .!= [])
    "jmp" ->
      Jmp . Prelude.head <$> o .: "labels"
    "br" ->
      Br . Prelude.head <$> (o .: "args") <*> (Prelude.head <$> (o .: "labels")) <*> ((!! 1) <$> o .: "labels")
    "ret" -> ret <$> o .:? "args"
    _ ->
      Binary <$> (Dest <$> o .: "type" <*> o .: "dest") <*> o .: "op" <*> (Prelude.head <$> (o .: "args")) <*> ((!! 1) <$> o .: "args")
      <|>
      Unary <$> (Dest <$> o .: "type" <*> o .: "dest") <*> o .: "op" <*> (Prelude.head <$> (o .: "args"))

instance FromJSON BinaryOperation where
  parseJSON (String s) =
    case s of
     "add" -> return Add
     "fadd" -> return FAdd
     "ptradd" -> return  PtrAdd
     "mul" -> return Mul
     "fmult" -> return FMul
     "sub" -> return  Sub
     "fsub" -> return FSub
     "div" -> return Div
     "fdiv" -> return FDiv
     "eq" -> return Eq
     "feq" -> return FEq
     "lt" -> return Lt
     "flt" -> return FLt
     "gt" -> return Gt
     "fgt" -> return FGt
     "le" -> return FLe
     "ge" -> return Ge
     "fge" -> return FGe
     "and" -> return And
     "or" -> return Or
     _ -> typeMismatch "Valid binary operation" $ String s

  parseJSON val =  typeMismatch "String" val

instance FromJSON UnaryOperation where
  parseJSON (String s) =
    case s of
      "not" -> return Not
      "id" -> return Id
      "load" -> return Load
      "alloc" -> return Alloc
      _ -> typeMismatch "Valid unary operation" $ String s

  parseJSON val  = typeMismatch "String" val


