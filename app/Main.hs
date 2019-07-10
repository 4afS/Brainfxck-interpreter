module Main where

import Brainfxck
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
    Just ":q" -> outputStrLn "see you later."
    Just s -> liftIO (run s) >> repl
    Nothing -> outputStrLn "Please Enter a code" >> repl
