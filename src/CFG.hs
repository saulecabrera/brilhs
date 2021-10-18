module CFG where

import Block (Block(..))
import Instr (Instr(..))
import Data.HashMap.Lazy as M
import Data.Text
import Data.Foldable (traverse_, foldl, Foldable (toList))
import Control.Monad.State

newtype CFG = CFG (M.HashMap Text [Block])
type BlockAddr = M.HashMap Text Block

data BuildState = BuildState { buildBlocksAddr :: BlockAddr
                             , buildBlocks :: [Block]
                             , buildIndex :: Int
                             , cfg :: CFG
                             } deriving(Show)

fromBlocks :: [Block] -> CFG
fromBlocks blocks = 
  cfg $ execState fromBlocks' BuildState { buildBlocksAddr = Data.Foldable.foldl blockAddress M.empty blocks
                                         , buildIndex = 0
                                         , cfg = CFG M.empty 
                                         , buildBlocks = blocks
                                         }

blockAddress addr b@(Block name _) = M.insert name b addr

fromBlocks' :: State BuildState ()
fromBlocks' = get >>= traverseBlocks

traverseBlocks :: BuildState -> State BuildState ()
traverseBlocks BuildState{buildBlocks = blocks} = traverse_ addBlock blocks

addBlock :: Block -> State BuildState ()
addBlock b@(Block name _) = modify $ \st@BuildState {buildIndex = idx, cfg = (CFG m), buildBlocksAddr = addr, buildBlocks = blocks} ->
  st {buildIndex = idx + 1
     , cfg = CFG $ M.insert name (successors b addr blocks idx) m
     }

successors :: Block -> BlockAddr -> [Block] -> Int -> [Block]
successors (Block _ []) _ _ _ =  []
successors (Block _ ins) addr blocks idx = 
  case Prelude.last ins of
    Jmp to -> [addr M.! to]
    Br _ t f -> [addr M.! t, addr M.! f]
    Ret _ -> []
    _ -> [blocks !! next | next < Prelude.length blocks]
      where
        next = idx + 1

instance Show CFG where
  show (CFG m) = Data.Foldable.foldl f "" (M.toList m)
    where
      f = \acc (k, bs) -> acc ++ "\n" ++ show k ++ "->" ++ blocks bs
      blocks bs = show $ Prelude.map (\(Block name _) -> name ) bs
      
