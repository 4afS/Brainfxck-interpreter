{-# LANGUAGE NoImplicitPrelude #-}

module Run
  ( sayHello
  ) where

import Import
import Prelude (putStrLn)

sayHello :: RIO String ()
sayHello = do
  name <- ask
  liftIO $ putStrLn $ "Hello, " <> name
