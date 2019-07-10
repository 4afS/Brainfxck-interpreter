{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Brainfxck
  ( run
  )
where

import           Control.Monad.State (StateT, evalStateT, get, lift, modify)
import           Data.Char           (chr, ord)
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

data Op
  = MoveRight
  | MoveLeft
  | Increment
  | Decrement
  | Put
  | Substitution
  | Loop Ops
  deriving (Eq, Show)

type Memory = (Vector Int, Pointer)
type Pointer = Int

type Ops = Vector Op

parse :: String -> Ops
parse []     = V.empty
parse source = fst $ parse' (V.empty, source)
 where
  parse' (vs, []         ) = (vs, [])
  parse' (vs, op : source) = case op of
    '>' -> parse' (vs `V.snoc` MoveRight, source)
    '<' -> parse' (vs `V.snoc` MoveLeft, source)
    '+' -> parse' (vs `V.snoc` Increment, source)
    '-' -> parse' (vs `V.snoc` Decrement, source)
    '.' -> parse' (vs `V.snoc` Put, source)
    ',' -> parse' (vs `V.snoc` Substitution, source)
    '[' ->
      let (sub, xss) = parse' (V.empty, source)
      in  parse' (vs `V.snoc` Loop sub, xss)
    ']' -> (vs, source)
    _ -> parse' (vs, source)

pattern Cons x xs <- (uncons -> Just (x, xs))
pattern Nil <- (uncons -> Nothing)

uncons :: Vector a -> Maybe (a, Vector a)
uncons (V.null -> True) = Nothing
uncons vs               = Just (V.head vs, V.tail vs)

toEvaluable :: Ops -> StateT Memory IO (Maybe String)
toEvaluable op = fmap (map chr) . sequence . filter (/= Nothing) <$> toEvaluable' op
  where
    toEvaluable' :: Ops -> StateT Memory IO [Maybe Int]
    toEvaluable' Nil                 = return []
    toEvaluable' (Cons (Loop op) xs) = loop op ++^ toEvaluable' xs
    toEvaluable' (Cons x         xs) = evaluate x +^ toEvaluable' xs

    loop :: Ops -> StateT Memory IO [Maybe Int]
    loop op = do
      (memory, pointer) <- get
      if memory V.! pointer > 0
         then toEvaluable' op ++^ loop op
         else return []

(+^) :: Monad m => m a -> m [a] -> m [a]
mx +^ mxs = (:) <$> mx <*> mxs

(++^) :: Monad m => m [a] -> m [a] -> m [a]
mxs ++^ mys = (++) <$> mxs <*> mys

evaluate :: Op -> StateT Memory IO (Maybe Int)
evaluate MoveRight = modify (updatePointer increment) >> return Nothing

evaluate MoveLeft = modify (updatePointer decrement) >> return Nothing

evaluate Increment = do
  (_, pointer) <- get
  modify $ updateMemory pointer increment
  return Nothing

evaluate Decrement = do
  (_, pointer) <- get
  modify $ updateMemory pointer decrement
  return Nothing

evaluate Put = do
  (memory, pointer) <- get
  return . Just $ memory V.! pointer

evaluate Substitution = do
  (_, pointer) <- get
  c             <- lift getChar
  modify $ updateMemory pointer (const (ord c))
  return Nothing

updateMemory
  :: Pointer -> (Int -> Int) -> Memory -> Memory
updateMemory index f (memory, pointer) =
  (memory V.// [(index, f (memory V.! index))], pointer)

updatePointer :: (Pointer -> Pointer) -> Memory -> Memory
updatePointer f (memory, pointer) = (memory, f pointer)

increment, decrement :: Int -> Int
increment = (+ 1)
decrement = subtract 1

run :: String -> IO ()
run source = do
  let initializedMemory = V.replicate 30000 0
  let evaluable = toEvaluable $ parse source
  evaluated <- evalStateT evaluable (initializedMemory, 0)
  mapM_ putStrLn evaluated
