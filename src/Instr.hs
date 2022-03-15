{-# LANGUAGE OverloadedStrings #-}
module Instr
  ( Instr(..)
  , BinaryOperation(..)
  , UnaryOperation(..)
  , ret
  , terminator
  , args
  , dest
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

dest :: Instr -> Maybe Dest
dest (Const d _) = Just d
dest (Binary d _ _ _) = Just d
dest (Unary d _ _ ) = Just d
dest (Call d _ _) = d
dest (Phi d _ _) = Just d
dest _ = Nothing


instance FromJSON Instr where
  parseJSON = withObject "instr" $ \o -> F.asum [
                                                Label <$> o .: "label"
                                              , parseOp o
                                              ]

instance ToJSON Instr where
  toJSON (Label s) = object ["label" .= s]
  toJSON (Const d l) = object ["const" .= object ["dest" .= d, "literal" .= l]]
  toJSON (Binary d op a1 a2) = object ["binary" .= object ["dest" .= d, "op" .= op, "arg1" .= a1, "arg2" .= a2]]
  toJSON (Unary d op a) = object ["unary" .= object ["dest" .= d, "op" .= op, "arg" .= a]]
  toJSON (Jmp s) = object ["jmp" .= s]
  toJSON (Br a s1 s2) = object ["br" .= object ["arg" .= a, "true" .= s1, "false" .= s2]]
  toJSON (Ret a) = object ["ret" .= a]
  toJSON (Call d i a) = object ["call" .= object ["dest" .= d, "ident" .= i, "args" .= a]]
  toJSON (Store a1 a2) = object ["store" .= object ["arg1" .= a1, "arg2" .= a2]]
  toJSON (Free a) = object ["free" .= a]
  toJSON Speculate = object ["speculate" .= (1 :: Int)]
  toJSON Commit = object ["commit" .= (1 :: Int)]
  toJSON (Guard a s) = object ["guard" .= object ["arg" .= a, "label" .= s]]
  toJSON (Phi d s a) = object ["phi" .= object ["dest" .= d, "args" .= a, "labels" .= s]]
  toJSON (Print a) = object ["print" .= a]
  toJSON Nop = object ["nop" .= (1 :: Int)]
  

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
     "le" -> return Le
     "fle" -> return FLe
     "ge" -> return Ge
     "fge" -> return FGe
     "and" -> return And
     "or" -> return Or
     _ -> typeMismatch "Valid binary operation" $ String s

  parseJSON val =  typeMismatch "String" val

instance ToJSON BinaryOperation where
  toJSON Add = String "add"
  toJSON FAdd = String "fadd"
  toJSON PtrAdd = String "ptradd"
  toJSON Mul = String "mul"
  toJSON FMul = String "fmult"
  toJSON Sub = String "sub"
  toJSON FSub = String "fsub"
  toJSON Div = String "div"
  toJSON FDiv = String "fdiv"
  toJSON Eq = String "eq"
  toJSON FEq = String "feq"
  toJSON Lt = String "lt"
  toJSON FLt = String "flt"
  toJSON Gt = String "gt"
  toJSON FGt = String "fgt"
  toJSON FLe = String "fle"
  toJSON Le = String "le"
  toJSON Ge = String "ge"
  toJSON FGe = String "fge"
  toJSON And = String "and"
  toJSON Or = String "or"

instance FromJSON UnaryOperation where
  parseJSON (String s) =
    case s of
      "not" -> return Not
      "id" -> return Id
      "load" -> return Load
      "alloc" -> return Alloc
      _ -> typeMismatch "Valid unary operation" $ String s

  parseJSON val  = typeMismatch "String" val

instance ToJSON UnaryOperation where
  toJSON Not = String "not"
  toJSON Id = String "id"
  toJSON Load = String "load"
  toJSON Alloc = String "alloc"

args :: Instr -> [Arg]
args (Binary _ _ a b) = [a, b]
args (Unary _ _ a) = [a]
args (Call _ _ as) = as
args (Store a b) = [a, b]
args (Free a) = [a]
args (Guard a _) = [a]
args (Phi _ _ as) = as
args (Print as) = as
args _ = []
