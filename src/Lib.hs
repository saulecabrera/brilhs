module Lib where

import Data.Text

type Ty = Text -- no parametrized types for now
type OptionalType = Maybe Ty

data Number = Floating Double | Integral Int

data Literal = Bol Bool
             | Number
             deriving (Show)

data Dest = Dest Ty Text deriving (Show)

type Arg = Text

newtype Program = Program [Fn]

data Fn = Fn Text [Dest] OptionalType [Instr]

data Instr = Label Text
           | Const Dest Literal
           | Binary Dest BinaryOperation
           | Unary Dest UnaryOperation
           | Control ControlOperation
           | Print [Arg]
           | Nop
           deriving (Show)

data BinaryOperation = Add
                     | Mul
                     | Sub
                     | Div
                     | Eq
                     | Lt
                     | Gt
                     | Le
                     | Ge
                     | And
                     | Or
                     deriving (Show)

data UnaryOperation = Not
                    | Id
                    deriving (Show)

data ControlOperation = Jmp Instr -- (Label l)
                      | Br Arg Instr Instr -- Br Arg (Label t) (Label f)
                      | Ret (Maybe Arg)
                      deriving (Show)

