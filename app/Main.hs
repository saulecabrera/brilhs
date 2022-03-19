{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Bril
import Optimizer (Passes (..))
import Program (Program)
import System.Environment
import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe (Maybe (..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Text
import Data.Functor ((<&>))
import Options.Applicative
import qualified Data.ByteString
import GHC.IO.Handle.FD (openFile)
import GHC.IO.IOMode (IOMode(ReadMode))
import System.IO (stdin, Handle)
import System.FilePath.Posix (takeBaseName)

data Emit = AST | CFG | JSON deriving (Show, Read)

data Options = Options { input :: FilePath
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
process opts@(Options f p e) = do
  handle <- handle f
  contents <- BSL.hGetContents handle

  case (eitherDecode contents :: Either String Program) of
    Right program -> do
      let 
        optimized = optimize program (passes opts)
        fileName = takeBaseName f
       in
        case e of
          AST -> print optimized
          CFG -> print $ cfg optimized
          JSON -> let json = encodePretty optimized in
                      writeFile (fileName ++ ".opt.json") (BSL8.unpack json)
    Left reason -> print reason

handle :: FilePath -> IO Handle
handle f = openFile f ReadMode

passes :: Options -> [Passes]
passes (Options _ (Just ps) _) = ps
passes Options {} = []


options :: Parser Options 
options = Options
  <$> strArgument (metavar "FILE" <> help "Input file")
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
  "cfg" -> Right CFG
  "json" -> Right JSON
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
