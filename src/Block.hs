{-# LANGUAGE OverloadedStrings #-}
module Block 
  ( Block(..)
  , instrs
  , appendInstr
  , anonymous
  )
  where

import Data.Text
import Instr (Instr)

data Block = Block Text [Instr] deriving (Show)

instrs :: Block -> [Instr]
instrs (Block _ i) = i

appendInstr :: Block -> Instr -> Block
appendInstr (Block n instrs) instr =  
  Block n (instrs ++ [instr])

anonymous :: [Instr] -> Block
anonymous = Block ""
