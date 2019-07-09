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
  | Loop (Vector Op)
  | InvalidOp
  deriving (Eq, Show)

type Watching = Int
type Sequence = Vector Int
type PointerSequence = (Sequence, Watching)

parse :: String -> Vector Op
parse []     = V.empty
parse source = fst $ parse' (V.empty, source)
 where
  parse' (vs, []           ) = (vs, [])
  parse' (vs, op : source) = case op of
    '>' -> parse' (vs `V.snoc` MoveRight, source)
    '<' -> parse' (vs `V.snoc` MoveLeft, source)
    '+' -> parse' (vs `V.snoc` Increment, source)
    '-' -> parse' (vs `V.snoc` Decrement, source)
    '.' -> parse' (vs `V.snoc` Put, source)
    ',' -> parse' (vs `V.snoc` Substitution, source)
    '[' -> let (sub, xss) = parse' (V.empty, source)
            in parse' (vs `V.snoc` Loop sub, xss)
    ']' -> (vs, source)

execute :: Vector Op -> StateT PointerSequence IO ()
execute = undefined

run :: String -> IO ()
run source = do
  let initialPointerSequence = V.replicate 30000 0 
  let parsedSource = parse source
  print =<< runStateT (execute parsedSource) (initialPointerSequence, 0)
