module CFG where

import Block (Block(..))
import Data.HashMap.Lazy as M
import Data.Text
import Control.Monad.State

newtype CFG = GFG (M.HashMap Text [Block])

data BuildState = BuildState { buildBlocks :: M.HashMap Text Block
                             , buildIndex :: Int
                             , cfg :: CFG
                             }
-- 
-- CFG
  -- Traverse the given blocks.
  -- For each block add an entry to the CFG state in the form of: (blockName -> successors)
-- Successors
  -- Check the last instruction of the block
  -- If the last instruction is `Jmp` or `Br`, search for the labels given as
  -- arguments to those instructions
  -- If the last instruction is `Ret`, there are no successors
  -- If the last instruction is not a terminator, 
--
-- State:
--    i. Map name -> block
--    ii. Map name -> [block] (CFG)
--    iii. The index that is currently being processed
--
-- Open questions:
--    i. Calculating fallthrough blocks
