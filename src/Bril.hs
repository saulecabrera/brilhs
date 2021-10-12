{-# LANGUAGE OverloadedStrings #-}
module Bril 
  ( formBlocks
  ) where

import Data.Foldable as F
import Program (Program(..))
import Instr (Instr(..), terminator)
import Fn (Fn(..))
import Block
  ( Block(..)
  , instrs
  , appendInstr
  , named
  , indexed
  , hasTerminator
  )

formBlocks :: Program -> [Block]
formBlocks (Program []) = []
formBlocks (Program fns) =
  -- TODO: Investigate the implications of processing all the functions
  case Prelude.head fns of
    Fn _ _ _ ins ->  reverse $ F.foldl formBlock [] (zip [0..length ins] ins)


formBlock :: [Block] -> (Int, Instr) -> [Block]
formBlock blocks@(current:rest) (idx, instr) =
  case instr of
    Label name -> named name:blocks
    _ -> appendInstr block instr:rest
      where
        block = if hasTerminator current
                   then indexed idx
                   else current

formBlock [] (_, Label name) = [named name]
formBlock [] (idx, instr) = [appendInstr (indexed idx) instr]

-- CFG calculation notes
--  Data structure needed to hold relationship between block names and blocks,
--  to make lookup O(1); another approach is to calculate successors inline,
--  when  creating the blocks. This would potentially work because the
--  program is assume to be valid at this stage (at least there shouldn't be
--  any missing or invalid labels). One of the main disadvantages of the approach mentioned above is block
--  lookup: given a label name, searching the corresponding block is at least
--  O(n)
--
--  Last instruction dictates successors: if the last instruction is `Jmp` or `Br`,
--  sucessors are the labels of each of the insructions
--  If the last instruction is `Ret`, there are no successors
