{-# LANGUAGE FlexibleContexts #-}
module Tandoori.Supply (Supply(Supply), getSupply) where

import Control.Monad.State

newtype Supply a = Supply{ unSupply :: [a] }

getSupply :: MonadState (Supply a) m => m a
getSupply = do x:xs <- gets unSupply
               put $ Supply xs
               return x
