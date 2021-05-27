{-# LANGUAGE OverloadedStrings #-}
module Bril where

import Data.Text
import Data.Aeson
import Data.HashMap.Strict as HM

-- Types
data TyKind = Primitive | Pointer deriving (Show)
data TyName = Int | Bool | Float deriving (Show)
data Ty = Ty TyKind TyName deriving (Show)
type OptionalType = Maybe Ty

-- Identifiers
type Ident = Text
type Arg = Text
type Label = Text

-- Literals
data Number = Floating Double | Integral Int
data Literal = Boolean Bool
             | Number
             deriving (Show)

-- Typed Identifier
data Dest = Dest Ty Ident deriving (Show)

-- Top level program
newtype Program = Program [Fn]

data Fn = Fn Ident [Dest] OptionalType [Instr] deriving (Show)

-- Instruction
data Instr = Lab Label
           | Const Dest Literal
           | Binary Dest BinaryOperation Arg Arg
           | Unary Dest UnaryOperation Arg
           | Control ControlOperation
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

data ControlOperation = Jmp Label
                      | Br Arg Label Label
                      | Ret (Maybe Arg)
                      deriving (Show)

primitiveOf = Ty Primitive
pointerOf = Ty Pointer

instance FromJSON Program where
  parseJSON = withObject "program" $ \o ->
    Program <$> o .: "functions"

instance FromJSON Fn where
  parseJSON = withObject "function" $ \o ->
    Fn <$> o .: "name"
       <*> o .: "args"
       <*> o .: "type"
       <*> o .: "instrs"

instance FromJSON Dest where
  parseJSON = withObject "dest" $ \o->
    Dest <$> o .: "type" <*> o .: "name"

instance FromJSON Ty where
  parseJSON (String "int") =
    return $ primitiveOf Int

  parseJSON (String "float") =
    return $ primitiveOf Float

  parseJSON (String "bool") =
    return $ primitiveOf Bril.Bool

  parseJSON (Object o) =
    return $ case HM.lookup "ptr" o of
               Just (String "int") -> primitiveOf Int
               Just (String "float") -> primitiveOf Float
               Just (String "bool") -> primitiveOf Bril.Bool




