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
  | InvalidOp
  deriving (Eq, Show)

type Watching = Int
type PointerSequence = (Vector Int, Watching)

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

toEvaluable :: Ops -> StateT PointerSequence IO (Maybe String)
toEvaluable op = fmap (map chr) . sequence . filter (/= Nothing) <$> toEvaluable' op
  where
    toEvaluable' :: Ops -> StateT PointerSequence IO [Maybe Int]
    toEvaluable' Nil                 = return []
    toEvaluable' (Cons (Loop op) xs) = loop op ++^ toEvaluable' xs
    toEvaluable' (Cons x         xs) = evaluate x +^ toEvaluable' xs

    loop :: Ops -> StateT PointerSequence IO [Maybe Int]
    loop op = do
      (seq, watching) <- get
      if seq V.! watching > 0
         then toEvaluable' op ++^ loop op
         else return []

(+^) :: Monad m => m a -> m [a] -> m [a]
mx +^ mxs = (:) <$> mx <*> mxs

(++^) :: Monad m => m [a] -> m [a] -> m [a]
mxs ++^ mys = (++) <$> mxs <*> mys

evaluate :: Op -> StateT PointerSequence IO (Maybe Int)
evaluate MoveRight = do
  modify $ updateWatching increment
  return Nothing

evaluate MoveLeft = do
  modify $ updateWatching decrement
  return Nothing

evaluate Increment = do
  (_, watching) <- get
  modify $ updatePointerSequence watching increment
  return Nothing

evaluate Decrement = do
  (_, watching) <- get
  modify $ updatePointerSequence watching decrement
  return Nothing

evaluate Put = do
  (seq, watching) <- get
  return . Just $ seq V.! watching

evaluate Substitution = do
  (_, watching) <- get
  c             <- lift getChar
  modify $ updatePointerSequence watching (const (ord c))
  return Nothing

updatePointerSequence
  :: Watching -> (Int -> Int) -> PointerSequence -> PointerSequence
updatePointerSequence index f (seq, watching) =
  (seq V.// [(index, f (seq V.! index))], watching)

updateWatching :: (Watching -> Watching) -> PointerSequence -> PointerSequence
updateWatching f (seq, watching) = (seq, f watching)

increment, decrement :: Int -> Int
increment = (+ 1)
decrement = subtract 1

run :: String -> IO ()
run source = do
  let initializedPointerSequence = V.replicate 30000 0
  let evaluable = toEvaluable $ parse source
  evaluated <- evalStateT evaluable (initializedPointerSequence, 0)
  mapM_ putStrLn evaluated
