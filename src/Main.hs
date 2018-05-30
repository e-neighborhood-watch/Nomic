module Main where

import Data.List

import Language.Haskell.Interpreter

import System.IO
import System.IO.Temp

main :: IO ()
main = emptySystemTempFile "source.hs" >>= mainLoop

mainLoop :: FilePath -> IO ()
mainLoop fp = do
  putStrLn "Enter program below (empty line to end):"
  readTillEmptyLine >>= writeFile fp
  putStrLn "Processing program..."
  result <- runInterpreter $ runUserFile fp
  case result of
    Left err  -> putStrLn $ errorString err
    Right str -> putStrLn str >> mainLoop fp

readTillEmptyLine :: IO String
readTillEmptyLine = do
  line <- getLine
  case line of
    "" -> return ""
    _  -> (line++) . ('\n':) <$> readTillEmptyLine

errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

runUserFile :: FilePath -> Interpreter String
runUserFile fp = loadModules [fp] >> getLoadedModules >>= setTopLevelModules >> interpret "game" infer
