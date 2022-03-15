module Optimizer
  ( dce
  , Passes (..)
  ) where

import Control.Monad.State
import Data.Foldable (traverse_, foldl')
import Data.Functor ((<&>))
import Id
  (Arg
  , Dest (..)
  )
import Instr
  ( Instr(..)
  , args
  , dest
  )
import Block
  (Block(..)
  , instrs
  , size
  )

data Passes = DCE deriving (Show, Read)

dce :: [Block] -> [Block]
dce blocks = let result = dce' blocks
             in if any ((== False) . fst) result
                  then map snd result
                  else dce (map snd result)

dce' :: [Block] -> [(Bool, Block)]
dce' blocks = map (optimize $ usages blocks) blocks

usages :: [Block] -> [Arg]
usages = concatMap usage

usage :: Block -> [Arg]
usage b = concatMap args (instrs b)

optimize :: [Arg] -> Block -> (Bool, Block)
optimize usages b@(Block name ins) = (changed, block)
  where
    block = Block name optimized 
    optimized = filter (\i -> 
        case dest i of
          Nothing -> True
          Just (Dest _ id) -> id `elem` usages
      ) ins
    changed = size b /= size block

