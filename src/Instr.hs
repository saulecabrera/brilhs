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

import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Foldable       as F
import           Data.Text
import           Id                  (Arg, Dest (..), Ident, Literal,
                                      optionalDest)
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
ret (Just _)   = Ret Nothing

terminator :: Instr -> Bool
terminator instr =
  case instr of
    Jmp _ -> True
    Ret _ -> True
    Br {} -> True
    _     -> False

dest :: Instr -> Maybe Dest
dest (Const d _)      = Just d
dest (Binary d _ _ _) = Just d
dest (Unary d _ _ )   = Just d
dest (Call d _ _)     = d
dest (Phi d _ _)      = Just d
dest _                = Nothing


instance FromJSON Instr where
  parseJSON = withObject "instr" $ \o -> do
    label <- o .:? "label"
    case label of
      Just l -> return $ Label l
      _      -> parseOp o

instance ToJSON Instr where
  toJSON (Label s) = object ["label" .= s]
  toJSON (Const (Dest ty id) l) = object ["op" .= String "const", "dest" .= id, "type" .= ty, "value" .= l ]
  toJSON (Binary (Dest ty id) op a1 a2) = object [ "op" .= op, "dest" .= id, "type" .= ty, "args" .= [a1, a2] ]
  toJSON (Unary (Dest ty id) op a) = object [ "op" .= op, "dest" .= id, "type" .= ty, "args" .= [a] ]
  toJSON (Jmp s) = object [ "op" .= String "jmp", "labels" .= [s] ]
  toJSON (Br a s1 s2) = object [ "op" .= String "br", "args" .= [a], "labels" .= [s1, s2] ]
  toJSON (Ret a) = object [ "op" .= String "ret", "args" .= [a] ]
  toJSON (Call (Just (Dest ty id)) i a) = object [ "op" .= String "call", "dest" .= id, "type" .= ty, "args" .= [a], "funcs" .= [i] ]
  toJSON (Call Nothing i a) = object [ "op" .= String "call", "dest" .= Null, "type" .= Null, "args" .= [a], "funcs" .= [i] ]
  toJSON (Store a1 a2) = object [ "op" .= String "store", "args" .= [a1, a2] ]
  toJSON (Free a) = object [ "op" .= String "free", "args" .= [a] ]
  toJSON Speculate = object [ "op" .= String "speculate" ]
  toJSON Commit = object [ "op" .= String "commit" ]
  toJSON (Guard a s) = object [ "op" .= String "guard", "args" .= [a], "labels" .= [s] ]
  toJSON (Phi (Dest ty id) s a) = object [ "op" .= String "phi", "dest" .= id, "type" .= ty, "args" .= [a], "labels" .= [s] ]
  toJSON (Print a) = object [ "op" .= String "print", "args" .= a ]
  toJSON Nop = object [ "op" .= String "nop" ]


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
     "add"    -> return Add
     "fadd"   -> return FAdd
     "ptradd" -> return  PtrAdd
     "mul"    -> return Mul
     "fmult"  -> return FMul
     "sub"    -> return  Sub
     "fsub"   -> return FSub
     "div"    -> return Div
     "fdiv"   -> return FDiv
     "eq"     -> return Eq
     "feq"    -> return FEq
     "lt"     -> return Lt
     "flt"    -> return FLt
     "gt"     -> return Gt
     "fgt"    -> return FGt
     "le"     -> return Le
     "fle"    -> return FLe
     "ge"     -> return Ge
     "fge"    -> return FGe
     "and"    -> return And
     "or"     -> return Or
     _        -> typeMismatch "Valid binary operation" $ String s

  parseJSON val =  typeMismatch "String" val

instance ToJSON BinaryOperation where
  toJSON Add    = String "add"
  toJSON FAdd   = String "fadd"
  toJSON PtrAdd = String "ptradd"
  toJSON Mul    = String "mul"
  toJSON FMul   = String "fmult"
  toJSON Sub    = String "sub"
  toJSON FSub   = String "fsub"
  toJSON Div    = String "div"
  toJSON FDiv   = String "fdiv"
  toJSON Eq     = String "eq"
  toJSON FEq    = String "feq"
  toJSON Lt     = String "lt"
  toJSON FLt    = String "flt"
  toJSON Gt     = String "gt"
  toJSON FGt    = String "fgt"
  toJSON FLe    = String "fle"
  toJSON Le     = String "le"
  toJSON Ge     = String "ge"
  toJSON FGe    = String "fge"
  toJSON And    = String "and"
  toJSON Or     = String "or"

instance FromJSON UnaryOperation where
  parseJSON (String s) =
    case s of
      "not"   -> return Not
      "id"    -> return Id
      "load"  -> return Load
      "alloc" -> return Alloc
      _       -> typeMismatch "Valid unary operation" $ String s

  parseJSON val  = typeMismatch "String" val

instance ToJSON UnaryOperation where
  toJSON Not   = String "not"
  toJSON Id    = String "id"
  toJSON Load  = String "load"
  toJSON Alloc = String "alloc"

args :: Instr -> [Arg]
args (Binary _ _ a b) = [a, b]
args (Unary _ _ a)    = [a]
args (Call _ _ as)    = as
args (Store a b)      = [a, b]
args (Free a)         = [a]
args (Guard a _)      = [a]
args (Phi _ _ as)     = as
args (Print as)       = as
args (Br a _ _)       = [a]
args _                = []
