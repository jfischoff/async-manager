module Main where
import Control.Concurrent.AsyncManager
import GHC.Conc
import Debug.Trace
import Control.Monad 

main = do
  traceEventIO "start"
  manager <- newAsyncManager
  traceEventIO "created manager"
  t1 <- labelAsyncWithManager manager "test thread 1" $ forever $ return () >> threadDelay 500000
  traceEventIO "created first thread"
  t2 <- labelAsyncWithManager manager "test thread 2" $ forever $ return () >> threadDelay 500000
  traceEventIO "created second thread"
  traceEventIO "before clear"
  clear manager
  traceEventIO "after clear"
  threadDelay 500000
  traceEventIO "Done"