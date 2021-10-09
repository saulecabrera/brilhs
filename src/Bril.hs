{-# LANGUAGE OverloadedStrings #-}
module Bril 
  ( formBlocks
  ) where

import Data.Foldable as F
import Program (Program(..))
import Instr (Instr(..), terminator)
import Fn (Fn(..))
import Block (Block(..), instrs, appendInstr, anonymous)

formBlocks :: Program -> [Block]
formBlocks (Program []) = []
formBlocks (Program fns) =
  case Prelude.head fns of
    Fn _ _ _ ins ->  reverse $ F.foldl formBlock [] ins


formBlock :: [Block] -> Instr -> [Block]
formBlock (current:blocks) instr =
  case instr of
    Label name -> Block name [instr]:(current:blocks)
    _ ->
      if terminator $ Prelude.last $ instrs current
         then anonymous [instr]:(current:blocks)
       else appendInstr current instr:blocks

formBlock [] i@(Label name) = [Block name [i]]
formBlock [] instr = [anonymous [instr]]
