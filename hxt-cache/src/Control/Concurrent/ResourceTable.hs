-----------------------------------------------------------------------------

module Control.Concurrent.ResourceTable
where

import           Control.Concurrent.MVar

import qualified Data.Map                       as M
import           Data.Maybe

-----------------------------------------------------------------------------

type ResourceTable a    = MVar (M.Map a ResourceLock)
type ResourceLock       = (MVar (), Int)

-----------------------------------------------------------------------------

requestResource         :: (Ord a) => ResourceTable a -> a -> IO ()
requestResource theLocks r
                        = do
                          rt <- takeMVar theLocks
                          (lk, cnt) <- case M.lookup r rt of
                                       Nothing          -> do
                                                           lk' <- newMVar ()
                                                           return (lk', 0)
                                       Just l           -> return l
                          putMVar theLocks $ M.insert r (lk, cnt + 1) rt
                          takeMVar lk

releaseResource         :: (Ord a) => ResourceTable a -> a -> IO ()
releaseResource theLocks r
                        = do
                          rt <- takeMVar theLocks
                          let (lk, cnt) = fromJust . M.lookup r $ rt
                          putMVar theLocks $ if cnt == 1
                                             then M.delete r rt
                                             else M.insert r (lk, cnt - 1) rt
                          putMVar lk ()

newResourceTable        :: IO (ResourceTable a)
newResourceTable        = newMVar M.empty

{-# NOINLINE newResourceTable #-}

-----------------------------------------------------------------------------
