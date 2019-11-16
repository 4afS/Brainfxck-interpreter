module Main where

import Brainfxck
import CheckSyntax
import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline

type Repl a = InputT IO a

main :: IO ()
main = runInputT defaultSettings repl

repl :: Repl ()
repl = do
  safeInput <- getInputLine ">> "
  case safeInput of
    Just ":q" -> outputStrLn "See you later."
    Nothing -> outputStrLn "See you later" >> repl
    Just input -> liftIO (printExecuted =<< execute input) >> repl

printExecuted :: Either [Result] String -> IO ()
printExecuted (Left errors) = mapM_ print errors
printExecuted (Right s) = putStrLn s
