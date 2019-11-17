{-# LANGUAGE NoImplicitPrelude #-}

module Brainfxck.Tokenize
  ( tokenize
  ) where

import Import
import qualified RIO.Vector.Boxed as VB

tokenize :: String -> Tokens
tokenize = VB.filter (not . isComment) . VB.map toToken . VB.fromList

toToken :: Char -> Token
toToken '>' = GraterThan
toToken '<' = LessThan
toToken '+' = Plus
toToken '-' = Minus
toToken '.' = Dot
toToken ',' = Comma
toToken '[' = BracketLeft
toToken ']' = BracketRight
toToken _ = Comment

isComment :: Token -> Bool
isComment Comment = True
isComment _ = False
