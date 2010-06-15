-- |
-- general usefull state monad for a local state and IO
--
-- This module is used for threading a state and io actions
-- through a computation.
-- Here the monad is constructed by hand.
-- It could easily be refactored with the use of the monad transformer library.


module Control.Monad.MonadStateIO
    ( module Control.Monad.MonadStateIO
    )
where

import System.IO

-- ------------------------------------------------------------

newtype StateIO state res = STIO { trans :: state -> IO (res, state) }

instance Monad (StateIO state) where
    return v
        = STIO ( \s -> return (v, s))

    STIO cmd >>= f
        = STIO ( \s ->
                  cmd s >>= \ (rl, s') ->
                  (trans . f) rl s'
                )

-- ------------------------------------------------------------
-- |
-- lift IO command to 'StateIO'

io :: IO a -> StateIO state a
io iocmd
    = STIO ( \s ->
             do
             a <- iocmd
             return (a, s)
           )

-- ------------------------------------------------------------

-- |
-- state inspection command: a \"show\"-like function is applied to the state
-- and the result is written to stderr.

trcState        :: (state -> String) -> StateIO state ()
trcState fct
    = STIO ( \s ->
              do
              hPutStr stderr $ fct s
              return ((), s)
            )

-- ------------------------------------------------------------
--
-- state access commands

-- |
-- change the state with a given function and return the new state

changeState     :: (state -> state) -> StateIO state state
changeState f
    = STIO ( \s ->
             let s' = f s
             in
             return (s', s')
            )

-- |
-- set the state

setState        :: state -> StateIO state state
setState s
    = changeState ( \_ -> s )

-- |
-- read the state

getState        :: StateIO state state
getState
    = changeState id

-- ------------------------------------------------------------
-- |
-- run a 'StateIO' command with an initial state

run :: state -> StateIO state res -> IO res
run initialState (STIO cmd)
    = do
      (res, _finalState) <- cmd initialState
      return res


-- ------------------------------------------------------------
