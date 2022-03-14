{-# LANGUAGE OverloadedStrings #-}
module Bril 
  ( formBlocks
  , cfg
  , optimize
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
import qualified CFG
import qualified Optimizer

-- When processing multiple functions
-- extend the CFG to be named, 1 per function
-- at that point, this function should return an array of cfgs
cfg :: Program -> CFG.CFG
cfg (Program ((Fn _ _ _ ins):_)) =
  CFG.fromBlocks blocks
    where
      blocks = formBlocks ins

cfg (Program []) =
  CFG.fromBlocks []


formBlocks :: [Instr] -> [Block]
formBlocks ins =
    reverse $ F.foldl formBlock [] (zip [0..length ins] ins)

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

optimize :: Program -> [Optimizer.Passes] -> Program
optimize (Program []) _ = Program []
optimize p [] = p
optimize (Program ((Fn i d t ins): _)) _ = Program [Fn i d t instrs']
  where
    instrs' = concatMap (\(Block _ i) -> i) optimized
    optimized = Optimizer.dce $ formBlocks ins
