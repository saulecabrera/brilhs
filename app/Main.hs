{-# LANGUAGE OverloadedStrings #-}
module Main where
import Bril
import Program (Program)
import System.Environment
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import Data.Text
import Options.Applicative
import qualified Data.ByteString
import GHC.IO.Handle.FD (openFile)
import GHC.IO.IOMode (IOMode(ReadMode))
import System.IO (stdin)

data Level = Program | Blocks | CFG deriving (Show, Read)

data Input = File FilePath | Stdin

data Options = Options { input :: Input
                       , level :: Level
                       }

main :: IO ()
main = process =<< execParser opts
  where 
    opts = info (options <**> helper)
      ( fullDesc
      <> progDesc "Bril Intermediate Language")

process :: Options -> IO ()
process (Options f l) = do
  handle <- case f of 
              File p -> openFile p ReadMode
              Stdin -> return stdin

  contents <- BSL.hGetContents handle

  case (eitherDecode contents :: Either String Program) of
    Right program -> do
      case l of 
        Program -> print program
        Blocks ->
          print $ formBlocks program
        CFG ->
          print $ cfg program
    Left reason -> print reason

options :: Parser Options 
options = Options
  <$> inputParser
  <*> option auto
      ( long "level"
      <> short 'l'
      <> showDefault 
      <> value CFG
      <> help "Representation level")

inputParser = fileInput <|> stdinInput

fileInput :: Parser Input
fileInput = File <$> strOption
  ( long "file"
  <> short 'f'
  <>  metavar "FILENAME"
  <> help "Input file")

stdinInput :: Parser Input
stdinInput = flag' Stdin
  ( long "stdin"
  <> help "Read from stdin")
