{-# LANGUAGE OverloadedStrings #-}
module Bril
  ( Program, formBlocks ) where

import Data.Text
import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict as HM
import Data.Foldable as F
import Data.Scientific
import Control.Applicative ((<|>))
import Control.Monad (mzero)

-- Types
data Ty = Int | Bool | Float | Pointer Ty deriving (Show)
type OptionalType = Maybe Ty

-- Identifiers
type Ident = Text
type Arg = Text
type Label = Text

-- Literals
data Literal = Boolean Bool
             | Number Scientific
             deriving (Show)

-- Typed Identifier
data Dest = Dest Ty Ident deriving (Show)

-- Top level program
newtype Program = Program [Fn] deriving (Show)

data Fn = Fn Ident [Dest] OptionalType [Instr] deriving (Show)

-- Instruction
data Instr = Lab Label
           | Const Dest Literal
           | Binary Dest BinaryOperation Arg Arg
           | Unary Dest UnaryOperation Arg
           | Jmp Label
           | Br Arg Label Label
           | Ret (Maybe Arg)
           | Call (Maybe Dest) Ident [Arg]
           | Store Arg Arg
           | Free Arg
           | Speculate
           | Commit
           | Guard Arg Label
           | Phi Dest [Label] [Arg]
           | Print [Arg]
           | Nop
           deriving (Show)

-- Binary Value Operations
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
                     deriving (Show)

-- Unary Value Operations
data UnaryOperation = Not
                    | Id
                    | Load
                    | Alloc
                    deriving (Show)

pointerOf = Pointer

mkOptionalDest :: Maybe Ty -> Maybe Ident -> Maybe Dest
mkOptionalDest _ Nothing              = Nothing
mkOptionalDest Nothing _              = Nothing
mkOptionalDest (Just ty) (Just ident) = Just $ Dest ty ident

mkRet :: Maybe [Maybe Arg] -> Instr
mkRet Nothing    = Ret Nothing
mkRet (Just [])  = Ret Nothing
mkRet (Just [a]) = Ret a

-- JSON instances

instance FromJSON Program where
  parseJSON = withObject "program" $ \o ->
    Program <$> o .: "functions"

instance FromJSON Fn where
  parseJSON = withObject "function" $ \o ->
    Fn <$> o .: "name"
       <*> (o .:? "args" >>= maybe (pure []) pure)
       <*> o .:? "type"
       <*> o .: "instrs"

instance FromJSON Dest where
  parseJSON = withObject "dest" $ \o->
    Dest <$> o .: "type" <*> o .: "name"

instance FromJSON Ty where
  parseJSON (String "int") =
    return Int

  parseJSON (String "float") =
    return Float

  parseJSON (String "bool") =
    return Bril.Bool

  parseJSON (Object o) =
    case HM.lookup "ptr" o of
       Just (String "int") -> return $ pointerOf Int
       Just (String "float") -> return $ pointerOf Float
       Just (String "bool") -> return $ pointerOf Bril.Bool
       Just o -> do
         ty <- parseJSON o
         return $ pointerOf ty

instance FromJSON Instr where
  parseJSON = withObject "instr" $ \o -> F.asum [
                                                Lab <$> o .: "label"
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
      Call <$> (mkOptionalDest <$> o .:? "type" <*> o .:? "dest") <*> (Prelude.head <$> o .: "funcs") <*> o .: "args"
    "jmp" ->
      Jmp . Prelude.head <$> o .: "labels"
    "br" ->
      Br . Prelude.head <$> (o .: "args") <*> (Prelude.head <$> (o .: "labels")) <*> ((!! 1) <$> o .: "labels")
    "ret" -> mkRet <$> o .:? "args"
    _ ->
      Binary <$> (Dest <$> o .: "type" <*> o .: "dest") <*> o .: "op" <*> (Prelude.head <$> (o .: "args")) <*> ((!! 1) <$> o .: "args")
      <|>
      Unary <$> (Dest <$> o .: "type" <*> o .: "dest") <*> o .: "op" <*> (Prelude.head <$> (o .: "args"))


instance FromJSON Literal where
  parseJSON (Data.Aeson.Types.Bool b) =
    return $ Boolean b

  parseJSON (Data.Aeson.Types.Number s) =
    return $ Bril.Number s

instance FromJSON BinaryOperation where
  parseJSON (String s) =
    case s of
     "add" -> return Add
     "fadd" -> return FAdd
     "ptradd" -> return  PtrAdd
     "mul" -> return Mul
     "fmul" -> return FMul
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
     _ -> mzero

instance FromJSON UnaryOperation where
  parseJSON (String s) =
    case s of
      "not" -> return Not
      "id" -> return Id
      "load" -> return Load
      "alloc" -> return Alloc
      _ -> mzero


formBlocks :: Program -> [[Instr]]
formBlocks (Program []) = []
formBlocks (Program fns) =
  case Prelude.head fns of
    Fn _ _ _ ins ->  F.foldl formBlock [] ins


formBlock :: [[Instr]] -> Instr -> [[Instr]]
formBlock (current:blocks) instr =
  case instr of
    Lab _ -> [instr]:(current:blocks)
    Jmp _ -> []:(current ++ [instr]):blocks
    Ret _ -> []:(current ++ [instr]):blocks
    Br {} -> []:(current ++ [instr]):blocks
    _ -> (current ++ [instr]):blocks

formBlock [] instr = [[instr]]
