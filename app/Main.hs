{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Bril
import Optimizer (Passes (..))
import Program (Program)
import System.Environment
import Data.Aeson (eitherDecode)
import Data.Maybe (Maybe (..))
import qualified Data.ByteString.Lazy as BSL
import Data.Text
import Data.Functor ((<&>))
import Options.Applicative
import qualified Data.ByteString
import GHC.IO.Handle.FD (openFile)
import GHC.IO.IOMode (IOMode(ReadMode))
import System.IO (stdin)

data Emit = AST | Blocks | CFG deriving (Show, Read)

data Options = Options { input :: Maybe FilePath
                       , optimizations :: Maybe [Passes]
                       , emit :: Emit
                       }
main :: IO ()
main = process =<< execParser opts
  where 
    opts = info (options <**> helper)
      ( fullDesc
      <> progDesc "Bril Intermediate Language")

process :: Options -> IO ()
process (Options f passes e) = do
  handle <- case f of 
              Just p -> openFile p ReadMode
              _ -> return stdin

  contents <- BSL.hGetContents handle

  case (eitherDecode contents :: Either String Program) of
    Right program -> do
      let 
        opts = case passes of
          Just ps -> ps
          _ -> []
        optimized = optimize program opts in
        case e of
          AST -> print optimized
          Blocks -> undefined --print $ show $ blocks optimized
          CFG -> print $ cfg optimized
    Left reason -> print reason

options :: Parser Options 
options = Options
  <$> optional (strArgument (metavar "FILE" <> help "Input file"))
  <*> optimizationsParser
  <*> emitParser

emitParser :: Parser Emit
emitParser = option emitReader
  ( long "emit"
  <> short 'e'
  <> showDefault 
  <> value CFG
  <> help "Representation to emit") 

emitReader :: ReadM Emit
emitReader = eitherReader $ \s -> case s of
  "ast" -> Right AST
  "blocks" -> Right Blocks
  "cfg" -> Right CFG
  _ -> Left $ "Invalid representation: " ++ s

optimizationsParser :: Parser (Maybe [Passes])
optimizationsParser = optional $ option optimizationsReader
  ( long "opt"
  <> short 'O'
  <> help "Optimization passes") 

optimizationsReader :: ReadM [Passes]
optimizationsReader = str >>= \s -> traverse toPass (splitOn  "," s)
  where
    toPass = \case
      "dce" -> return DCE
      x -> readerError $ "Invalid optimization pass " ++ show x
