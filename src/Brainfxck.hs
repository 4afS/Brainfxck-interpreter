{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Brainfxck where

import CheckSyntax (Result, checkInvalids, isInvalid)
import Control.Lens ((%~), (&), _1, _2)
import Control.Monad.State (StateT, evalStateT, get, lift, modify)
import Data.Char (chr, ord)
import Data.Maybe (maybe)
import Data.Vector (Vector)
import qualified Data.Vector as V

data Op
  = MoveRight
  | MoveLeft
  | Increment
  | Decrement
  | Put
  | Substitution
  | Loop Ops
  deriving (Eq, Show)

type Memory = Vector Int

type Pointer = Int

type Ops = Vector Op

data Return
  = Value Int
  | None
  deriving (Eq)

parse :: String -> Ops
parse [] = V.empty
parse source = fst $ parse' (V.empty, source)
  where
    parse' (vs, []) = (vs, [])
    parse' (vs, op:source) =
      case op of
        '>' -> parse' (vs `V.snoc` MoveRight, source)
        '<' -> parse' (vs `V.snoc` MoveLeft, source)
        '+' -> parse' (vs `V.snoc` Increment, source)
        '-' -> parse' (vs `V.snoc` Decrement, source)
        '.' -> parse' (vs `V.snoc` Put, source)
        ',' -> parse' (vs `V.snoc` Substitution, source)
        '[' ->
          let (sub, xss) = parse' (V.empty, source)
           in parse' (vs `V.snoc` Loop sub, xss)
        ']' -> (vs, source)
        _ -> parse' (vs, source)

pattern Cons x xs <- (uncons -> Just (x, xs))

pattern Nil <- (uncons -> Nothing)

uncons :: Vector a -> Maybe (a, Vector a)
uncons (V.null -> True) = Nothing
uncons vs = Just (V.head vs, V.tail vs)

toEvaluable :: Ops -> StateT (Memory, Pointer) IO String
toEvaluable op =
  maybe "" (map chr) . sequence . filter (/= Nothing) <$> toEvaluable' op
  where
    toEvaluable' :: Ops -> StateT (Memory, Pointer) IO [Maybe Int]
    toEvaluable' Nil = return []
    toEvaluable' (Cons (Loop op) xs) = loop op ++^ toEvaluable' xs
    toEvaluable' (Cons x xs) = evaluate x +^ toEvaluable' xs
    loop :: Ops -> StateT (Memory, Pointer) IO [Maybe Int]
    loop op = do
      (memory, pointer) <- get
      if memory V.! pointer > 0
        then toEvaluable' op ++^ loop op
        else return []

(+^) :: Monad m => m a -> m [a] -> m [a]
mx +^ mxs = (:) <$> mx <*> mxs

(++^) :: Monad m => m [a] -> m [a] -> m [a]
mxs ++^ mys = (++) <$> mxs <*> mys

evaluate :: Op -> StateT (Memory, Pointer) IO (Maybe Int)
evaluate MoveRight = moveRight >> return Nothing
evaluate MoveLeft = moveLeft >> return Nothing
evaluate Increment = increment >> return Nothing
evaluate Decrement = decrement >> return Nothing
evaluate Put = do
  (memory, pointer) <- get
  return . Just $ memory V.! pointer
evaluate Substitution = do
  pointer <- getPointer
  c <- lift getChar
  modify $ updateMemory pointer $ const $ ord c
  return Nothing

updateMemory ::
     Pointer -> (Int -> Int) -> (Memory, Pointer) -> (Memory, Pointer)
updateMemory index f (memory, pointer) =
  (memory V.// [(index, f (memory V.! index))], pointer)

moveRight, moveLeft :: Monad m => StateT (Memory, Pointer) m ()
moveRight = modify $ _2 %~ (+ 1)

moveLeft = modify $ _2 %~ subtract 1

increment, decrement :: Monad m => StateT (Memory, Pointer) m ()
increment = do
  pointer <- getPointer
  modify (updateMemory pointer (+ 1))

decrement = do
  pointer <- getPointer
  modify (updateMemory pointer (subtract 1))

getPointer :: Monad m => StateT (Memory, Pointer) m Pointer
getPointer = do
  (_, pointer) <- get
  return pointer

execute :: String -> IO (Either [Result] String)
execute source = do
  let initializedMemory = V.replicate 30000 0
  if isInvalid source
    then return $ Left $ checkInvalids source
    else Right <$>
         evalStateT (toEvaluable $ parse source) (initializedMemory, 0)
