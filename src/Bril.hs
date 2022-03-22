{-# LANGUAGE OverloadedStrings #-}
module Bril
  ( formBlocks
  , cfg
  , optimize
  ) where

import           Block         (Block (..), appendInstr, hasOriginalLabel,
                                hasTerminator, indexed, instrs, named)
import qualified CFG
import           Data.Foldable as F
import           Fn            (Fn (..))
import           Instr         (Instr (..), terminator)
import qualified Optimizer
import           Program       (Program (..))

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
    optimized = Optimizer.dcePass $ formBlocks ins
    instrs' = concatMap (\b@(Block name i) -> if hasOriginalLabel b then Label name : i else i) optimized
