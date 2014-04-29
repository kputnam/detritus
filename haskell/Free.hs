{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

import Control.Monad.State
import Control.Monad.Free
import Control.Monad.Free.TH
import Data.Map (Map)
import qualified Data.Map as M

data RedisCmd a
  = Get' String (Maybe String -> a)
  | Put' String String a
  | Multi' (RedisCmdM ()) a
  deriving Functor

type RedisCmdM
  = Free RedisCmd

makeFree ''RedisCmd

-- runDebug example
runDebug :: MonadIO m => RedisCmdM a -> m a
runDebug = iterM run
  where
  --run :: MonadIO m => RedisCmd (m a) -> m a
    run (Get' k f) = do
      liftIO $ putStrLn $ unwords ["GET", k]
      f . (\case "" -> Nothing; x -> Just x) =<< liftIO getLine
    run (Put' k v f) = do
      liftIO $ putStrLn $ unwords ["PUT", k, v]
      f
    run (Multi' tx f) = do
      liftIO $ putStrLn "MULTI"
      runDebug tx
      liftIO $ putStrLn "EXEC"
      f

type MapDB
  = Map String String

-- evalState (runTest example) $ M.fromList [("foo", "val")]
runTest :: MonadState MapDB m => RedisCmdM a -> m a
runTest = iterM run
  where
  --run :: MonadState MapDB m => RedisCmd (m a) -> m a
    run (Get' k f)    = f =<< gets (M.lookup k)
    run (Put' k v f)  = modify (M.insert k v) >> f
    run (Multi' tx f) = runTest tx >> f

example :: RedisCmdM ()
example = do
  val <- get' "foo"
  case val of
    Nothing -> return ()
    Just v  -> multi' $ do
      put' "foo-1" v
      put' "foo-2" v
