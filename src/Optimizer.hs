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
          Nothing          -> True
          Just (Dest _ id) -> id `elem` usages
      ) ins
    changed = Block.size b /= Block.size block



dcePlus :: [Block] -> [Block]
dcePlus blocks = let result = map (\b -> evalState (dcePlus' b) DefState { defs = M.empty, candidates = [] }) blocks
                  in if any ((== True) . fst) result
                        then dcePlus (map snd result)
                        else map snd result

dcePlus' :: Block -> S (Bool, Block)
dcePlus' b@(Block name ins) = traverse_ handleInstr (zip [0..] ins) *> gets candidates >>= removeCandidates
  where
    removeCandidates :: [Int] -> S (Bool, Block)
    removeCandidates candidates = let block = filter (\(index, _) -> index `notElem` candidates) (zip [0..] ins)
                                      changed = Block.size b /= length block
                                   in return (changed, Block name (map snd block))

handleInstr :: (Int, Instr) -> S ()
handleInstr (index, instr) = traverse_ handleArg (args instr) *> findCandidates (dest instr) index
  where
    handleArg :: Arg -> S ()
    handleArg arg = modify $ \s -> DefState { defs = M.delete arg (defs s)
                                            , candidates = candidates s
                                            }
    findCandidates :: Maybe Dest -> Int -> S ()
    findCandidates Nothing _ = pure ()
    findCandidates (Just (Dest _ id)) index = modify' $ \s -> DefState { defs = M.insert id index (defs s)
                                                                     , candidates = if id `M.member` defs s
                                                                                     then (defs s M.! id) : candidates s
                                                                                     else candidates s
                                                                     }
