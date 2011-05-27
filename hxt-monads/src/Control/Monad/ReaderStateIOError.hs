{-# OPTIONS -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances -XTypeSynonymInstances #-}

-- ------------------------------------------------------------

module Control.Monad.ReaderStateIOError
    ( module Control.Monad.ReaderStateIOError
    , mzero, mplus
    , ask, asks, local
    , get, gets, put, modify
    , throwError, catchError
    )
where

import          Control.Exception   (IOException)
import          Control.Monad
import          Control.Monad.Error
import          Control.Monad.Reader
import          Control.Monad.State

-- ------------------------------------------------------------

-- |
-- reader state io monad implemented directly without any monad transformers

newtype ReaderStateIOError env state err res = RSIOE { unRSIOE :: env -> state -> IO (Either err res, state) }

instance
    (ToError err) =>
    Monad (ReaderStateIOError env state err)
    where
    return v
        = RSIOE $
          \ _e s -> return (Right v, s)

    fail msg
        = RSIOE $
          \ _e s -> return (Left $ toError msg, s)

    RSIOE cmd >>= f
        = RSIOE $
          \ e s -> do
                   (r', s') <- cmd e s
                   case r' of
                     Left  e' -> return (Left e', s')
                     Right v' -> let
                                 RSIOE cmd2 = f v'
                                 in
                                 cmd2 e $! s'

instance
    (ToError err) =>
    MonadPlus (ReaderStateIOError env state err)
    where
    mzero
        = fail "mzero"
    RSIOE cmd1 `mplus` RSIOE cmd2
        = RSIOE $
          \ e s -> do
                   res@(r', s') <- cmd1 e s
                   case r' of
                     Left  _  -> cmd2 e s'
                     Right _  -> return res
                   

instance
    (ToError err, IOExcToError err) =>
    MonadIO (ReaderStateIOError env state err)
    where
    liftIO a
        = RSIOE $
          \ _e s -> do
                    r <- ( a >>= return . Right) `catchError` (\ e -> return (Left e))
                    case r of
                      Left msg -> return (Left $ ioExcToError msg, s)
                      Right v' -> return (Right v',                s)

instance
    (ToError err) =>
    MonadState state (ReaderStateIOError env state err)
    where
    get
        = RSIOE $
          \ _e  s -> return (Right s, s)

    put s
        = RSIOE $
          \ _e _s -> return (Right (), s)

instance
    (ToError err) =>
    MonadReader env (ReaderStateIOError env state err)
    where
    ask
        = RSIOE $
          \ e  s -> return (Right e, s)

    local f (RSIOE cmd)
        = RSIOE $
          \ e  s -> cmd (f e) s

instance
    (ToError err) =>
    MonadError err (ReaderStateIOError env state err)
    where
    throwError er
        = RSIOE $
          \ _e s -> return (Left er, s)

    catchError (RSIOE f) handler
        = RSIOE $
          \ e s -> do
                   (r', s') <- f e s
                   case r' of
                     Left  e' -> let RSIOE h = handler e' in h e s'
                     Right v' -> return $ (Right v', s')

-- ------------------------------------------------------------

modifyIO                :: (ToError err, IOExcToError err) =>
                           (state -> IO state) -> ReaderStateIOError env state err ()
modifyIO f              = do
                          s0 <- get
                          s1 <- liftIO (f s0)
                          put s1

runReaderStateIOError   :: ReaderStateIOError env state err res -> env -> state -> IO (Either err res, state)
runReaderStateIOError (RSIOE cmd)
    = cmd
  
-- ------------------------------------------------------------

class ToError err where
    toError :: String -> err

instance ToError String where
    toError = id

class IOExcToError err where
    ioExcToError :: IOException -> err

instance IOExcToError String where
    ioExcToError = show

-- ------------------------------------------------------------
