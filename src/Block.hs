{-# LANGUAGE OverloadedStrings #-}
module Block 
  ( Block(..)
  , instrs
  , appendInstr
  , named
  , indexed
  , hasTerminator
  )
  where

import Data.Text
import Instr (Instr, terminator)

data Block = Block Text [Instr] deriving (Show, Eq)

instrs :: Block -> [Instr]
instrs (Block _ i) = i

appendInstr :: Block -> Instr -> Block
appendInstr (Block n instrs) instr =  
  Block n (instrs ++ [instr])

named :: Text -> Block
named name = Block name []

indexed :: Int -> Block
indexed idx =
  Block name []
    where
      name = pack ("block_" ++ show idx)

hasTerminator :: Block -> Bool
hasTerminator (Block _ []) = False
hasTerminator (Block _ ins) = terminator $ Prelude.last ins

