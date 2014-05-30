{-# LANGUAGE ExistentialQuantification #-}
module Control.Concurrent.AsyncManager where
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Data.IORef
import Control.Monad
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Monoid
import Data.Maybe
import Control.Applicative
import Control.Arrow (second)
import GHC.Conc

-- | A existentially quantified wrapper for Async a
data AnyAsync = forall a. AnyAsync (Async a) 

-- | An Async thread manager. Keeps track of allocated and running
--   Async threads. Useful for ensure threads are cleaned up
data AsyncManager = AsyncManager 
  { childrenThreads  :: MVar (HashMap ThreadId AnyAsync) 
  , childrenManagers :: MVar [AsyncManager]
  }
  

-- | Create a new empty manager
newAsyncManager :: IO AsyncManager
newAsyncManager = AsyncManager <$> newMVar mempty <*> newMVar mempty

-- | Create a child manager
newChildManager :: AsyncManager -> IO AsyncManager
newChildManager (AsyncManager _ csRef) = modifyMVar csRef $ \cs -> do
  child <- newAsyncManager 
  return (child : cs, child)

-- | Insert a new thread into the manager
--   TODO: maybe I should check if the computation is still running
insert :: AsyncManager -> Async a -> IO ()
insert (AsyncManager ref _) as 
  = modifyMVar_ ref 
  $ return . (H.insert (asyncThreadId as) (AnyAsync as))
  
-- | Cancel all threads and empty the manager
clear :: AsyncManager -> IO ()
clear (AsyncManager ref csRef) = do 
   modifyMVar_ ref $ \xs -> do
     forM_ (toList xs) $ \(AnyAsync x) -> cancel x
     return mempty
   
   modifyMVar_ csRef $ \xs -> do
     forM_ xs $ \x -> clear x
     return mempty

-- | Thread count. Includes alive and dead threads
count :: AsyncManager -> IO Int
count (AsyncManager ref csRef) = do
  threadCount  <- H.size <$> readMVar ref
  managerCount <- length <$> readMVar csRef
  return $ threadCount + managerCount

-- | Remove references to dead threads
compact :: AsyncManager -> IO ()
compact (AsyncManager ref _) = modifyMVar_ ref $ 
  fmap H.fromList . filterM ((\(AnyAsync x) -> isJust <$> poll x) . snd) . H.toList

-- TODO deepCompact

-- | Cancel the thread and remove the entry from the manager
cancelWithManager :: AsyncManager 
                  -> Async a
                  -> IO ()
cancelWithManager (AsyncManager ref _) as = do
  cancel as
  modifyMVar_ ref $ return . H.delete (asyncThreadId as)

-- | Create a new thread and add it to the manager
asyncWithManager :: AsyncManager 
                 -> IO a 
                 -> IO (Async a)
asyncWithManager am act = do
  result <- async act
  insert am result 
  return result

-- Create a thread with ghc event label
labelAsyncWithManager :: AsyncManager
                      -> String
                      -> IO a
                      -> IO (Async a)
labelAsyncWithManager am label act = do
  as <- asyncWithManager am act
  labelThread (asyncThreadId as) label 
  return as
  






