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
    _ -> 
      if hasTerminator current
         then appendInstr (indexed idx) instr:blocks
         else appendInstr current instr:rest

formBlock [] (_, Label name) = [named name]
formBlock [] (idx, instr) = [appendInstr (indexed idx) instr]
