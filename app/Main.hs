{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main
  ) where

import Import
import Prelude (getLine)
import Run

main :: IO ()
main = do
  name <- getLine
  runRIO name sayHello
