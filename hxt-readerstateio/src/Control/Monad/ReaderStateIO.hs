{-# OPTIONS -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances #-}

-- ------------------------------------------------------------

module Control.Monad.ReaderStateIO
    ( module Control.Monad.ReaderStateIO
    )
where

import 		Control.Monad.Reader
import		Control.Monad.State

-- ------------------------------------------------------------

-- |
-- reader state io monad implemented directly without any monad transformers

newtype ReaderStateIO env state res = RSIO ( env -> state -> IO (res, state) )

instance Monad (ReaderStateIO env state) where
    return v
	= RSIO $ \ _e s -> return (v, s)

    RSIO cmd >>= f
	= RSIO $ \ e s -> do
			  (r', s') <- cmd e s
			  let RSIO cmd2 = f r'
			  s' `seq` cmd2 e s'

instance MonadIO (ReaderStateIO env state) where
    liftIO a
	= RSIO $ \ _e s -> do
			   r <- a
			   return (r, s)

instance MonadState state (ReaderStateIO env state) where
    get
	= RSIO $ \ _e  s -> return (s, s)

    put s
	= RSIO $ \ _e _s -> return ((), s)

instance MonadReader env (ReaderStateIO env state) where
    ask
	= RSIO $ \  e  s -> return (e, s)

    local f (RSIO cmd)
	= RSIO $ \  e  s -> cmd (f e) s

modifyIO		:: (state -> IO state) -> ReaderStateIO env state ()
modifyIO f		= do
			  s0 <- get
			  s1 <- liftIO (f s0)
			  put s1

runReaderStateIO	:: ReaderStateIO env state res	-> env -> state -> IO (res, state)
runReaderStateIO (RSIO cmd) e s
    = cmd e s
      
-- ------------------------------------------------------------
