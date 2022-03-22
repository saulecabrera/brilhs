module Optimizer
  ( dcePass
  , Passes (..)
  ) where

import           Block               (Block (..), instrs, size)
import           Control.Monad.State
import           Data.Foldable       (foldl', traverse_)
import           Data.Functor        ((<&>))
import qualified Data.HashMap.Lazy   as M
import           Id                  (Arg, Dest (..), Ident)
import           Instr               (Instr (..), args, dest)

import           Data.Text           (Text)

type S = State DefState

data DefState = DefState { defs       :: M.HashMap Text Int
                         , candidates :: [Int]
                         }

data Passes = DCE deriving (Show, Read)


dcePass :: [Block] -> [Block]
dcePass = dcePlus . dce

dce :: [Block] -> [Block]
dce blocks = let result = dce' blocks
             in if any ((== True) . fst) result
                  then dce (map snd result)
                  else map snd result

dce' :: [Block] -> [(Bool, Block)]
dce' blocks = map (f blocks) blocks
  where
    f = optimize . usages

usages :: [Block] -> [Arg]
usages = concatMap usage

usage :: Block -> [Arg]
usage = concatMap args . instrs

optimize :: [Arg] -> Block -> (Bool, Block)
optimize usages b@(Block name ins) = (changed, block)
  where
    block = Block name optimized
    optimized = filter (\i ->
        case dest i of
          Nothing          -> True
          Just (Dest _ id) -> id `elem` usages
      ) ins
    changed = Block.size b /= Block.size block


enumerate :: [b] -> [(Int, b)]
enumerate = zip [0..]

initState :: DefState
initState = DefState { defs = M.empty
                     , candidates = []
                     }

dcePlus :: [Block] -> [Block]
dcePlus blocks = let result = map (\b -> evalState (dcePlus' b) initState) blocks
                     blocks' = map snd result
                     changes = map fst result
                  in if or changes
                        then dcePlus blocks'
                        else blocks'

dcePlus' :: Block -> S (Bool, Block)
dcePlus' b@(Block name ins) = traverse_ handleInstr (enumerate ins) *> gets candidates >>= remove
  where
    remove :: [Int] -> S (Bool, Block)
    remove candidates = let blockInfo = filter ((`notElem` candidates) . fst) (enumerate ins)
                            block = Block name (map snd blockInfo)
                            changed = Block.size b /= Block.size block
                         in return (changed, block)

handleInstr :: (Int, Instr) -> S ()
handleInstr (index, instr) = traverse_ arg' (args instr) *> candidates' (dest instr) index
  where
    arg' :: Arg -> S ()
    arg' arg = modify $ \s -> DefState { defs = M.delete arg (defs s) -- `delete` will only have an effect if the definition exists
                                       , candidates = candidates s
                                       }
    candidates' :: Maybe Dest -> Int -> S ()
    candidates' Nothing _ = pure ()
    candidates' (Just (Dest _ id)) index = modify' $ \s -> DefState { defs = M.insert id index (defs s)
                                                                     , candidates = if id `M.member` defs s
                                                                                     then (defs s M.! id) : candidates s
                                                                                     else candidates s
                                                                     }
