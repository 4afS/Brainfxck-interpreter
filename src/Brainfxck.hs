module Brainfxck where

import           Data.Char
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Control.Monad.State
import           Control.Lens

data Op
  = MoveRight
  | MoveLeft
  | Increment
  | Decrement
  | Put
  | Substitution
  | Loop [Op]
  | InvalidOp
  deriving (Eq, Show)

run :: String -> IO ()
run = undefined
