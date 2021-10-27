{-# LANGUAGE OverloadedStrings #-}
module Main where
import Bril
import Program (Program)
import System.Environment
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import Data.Text
import Options.Applicative
import Data.Foldable (traverse_)

data Level = Program | Blocks | CFG deriving (Show, Read)

data Options = Options { input :: Text
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
  contents  <- BSL.readFile $ unpack f
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
  <$> argument str (metavar "INPUT")
  <*> option auto
      ( long "level"
      <> short 'l'
      <> showDefault 
      <> value CFG
      <> help "Representation level")
