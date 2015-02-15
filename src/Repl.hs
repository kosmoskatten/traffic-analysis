{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Repl (Repl, runRepl, liftIO) where

import Control.Applicative (Applicative)
import Control.Monad.State (StateT, MonadIO, liftIO, runStateT)
import Control.Monad.State.Class (MonadState)

newtype Repl s a =
  Repl { extractRepl :: StateT s IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState s)

runRepl :: Repl s a -> s -> IO (a, s)
runRepl repl = runStateT (extractRepl repl)
