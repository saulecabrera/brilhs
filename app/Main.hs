{-# LANGUAGE OverloadedStrings #-}
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
                       , optimizations :: [Passes]
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
      let optimized = optimize program passes in
        case e of
          AST -> print $ show optimized
          Blocks -> undefined --print $ show $ blocks optimized
          CFG -> print $ show $ cfg optimized
    Left reason -> print reason

options :: Parser Options 
options = Options
  <$> optional (strArgument (metavar "FILE" <> help "Input file"))
  <*> optimizationsParser
  <*> emitParser

emitParser = option str
  ( long "emit"
  <> short 'e'
  <> showDefault 
  <> value "cfg"
  <> help "Representation to emit") <&> validateEmit

optimizationsParser = many (option str
  ( long "optimizations"
  <> short 'O'
  <> help "Optimization passes")) <&> validateOptimizations

validateOptimizations :: [Text] -> [Passes]
validateOptimizations opts = Prelude.map (\o -> case o of
                                          "dce" -> DCE
                                          _ -> error $ "Unknown optimization: " ++ show o) opts

validateEmit :: Text -> Emit
validateEmit e = case e of
  "ast" -> AST
  "blocks" -> Blocks
  "cfg" -> CFG
  _ -> error $ "Invalid emit option: " ++ show e
