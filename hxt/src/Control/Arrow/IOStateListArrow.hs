{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- ------------------------------------------------------------

{- |
   Module     : Control.Arrow.IOStateListArrow
   Copyright  : Copyright (C) 2005-8 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe\@fh-wedel.de)
   Stability  : experimental
   Portability: portable

   Implementation of arrows with IO and a state

-}

-- ------------------------------------------------------------

module Control.Arrow.IOStateListArrow
    ( IOSLA(..)
    , liftSt
    , runSt
    )
where

import Prelude hiding (id, (.))

import Control.Category

import Control.Arrow
import Control.Arrow.ArrowExc
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowIO
import Control.Arrow.ArrowList
import Control.Arrow.ArrowNF
import Control.Arrow.ArrowTree
import Control.Arrow.ArrowState

import Control.DeepSeq
import Control.Exception                ( SomeException
                                        , try
                                        )

-- ------------------------------------------------------------

-- | list arrow combined with a state and the IO monad

newtype IOSLA s a b = IOSLA { runIOSLA :: s -> a -> IO (s, [b]) }

instance Category (IOSLA s) where
    id                  = IOSLA $ \ s x -> return (s, [x])      -- don't defined id = arr id, this gives loops during optimization
    {-# INLINE id #-}

    IOSLA g . IOSLA f   = IOSLA $ \ s x -> do
                                           (s1, ys) <- f s x
                                           sequence' s1 ys
                                           where
                                           sequence' s' []       = return (s', [])
                                           sequence' s' (x':xs') = do
                                                                   (s1', ys') <- g s' x'
                                                                   (s2', zs') <- sequence' s1' xs'
                                                                   return (s2', ys' ++ zs')

instance Arrow (IOSLA s) where
    arr f               = IOSLA $ \ s x -> return (s, [f x])
    {-# INLINE arr #-}

    first (IOSLA f)     = IOSLA $ \ s (x1, x2) -> do
                                                   (s', ys1) <- f s x1
                                                   return (s', [ (y1, x2) | y1 <- ys1 ])

    -- just for efficiency
    second (IOSLA g)    = IOSLA $ \ s (x1, x2) -> do
                                                  (s', ys2) <- g s x2
                                                  return (s', [ (x1, y2) | y2 <- ys2 ])

    -- just for efficiency
    IOSLA f *** IOSLA g = IOSLA $ \ s (x1, x2) -> do
                                                   (s1, ys1) <- f s  x1
                                                   (s2, ys2) <- g s1 x2
                                                   return (s2, [ (y1, y2) | y1 <- ys1, y2 <- ys2 ])

    -- just for efficiency
    IOSLA f &&& IOSLA g = IOSLA $ \ s x -> do
                                           (s1, ys1) <- f s  x
                                           (s2, ys2) <- g s1 x
                                           return (s2, [ (y1, y2) | y1 <- ys1, y2 <- ys2 ])



instance ArrowZero (IOSLA s) where
    zeroArrow           = IOSLA $ \ s -> const (return (s, []))
    {-# INLINE zeroArrow #-}


instance ArrowPlus (IOSLA s) where
    IOSLA f <+> IOSLA g = IOSLA $ \ s x -> do
                                           (s1, rs1) <- f s  x
                                           (s2, rs2) <- g s1 x
                                           return (s2, rs1 ++ rs2)

instance ArrowChoice (IOSLA s) where
    left (IOSLA f)      = IOSLA $ \ s -> either
                                         (\ x -> do
                                                 (s1, y) <- f s x
                                                 return (s1, map Left y)
                                         )
                                         (\ x -> return (s, [Right x]))

    right (IOSLA f)     = IOSLA $ \ s -> either
                                         (\ x -> return (s, [Left x]))
                                         (\ x -> do
                                                 (s1, y) <- f s x
                                                 return (s1, map Right y)
                                         )

instance ArrowApply (IOSLA s) where
    app                 = IOSLA $ \ s (IOSLA f, x) -> f s x
    {-# INLINE app #-}

instance ArrowList (IOSLA s) where
    arrL f              = IOSLA $ \ s x -> return (s, (f x))
    {-# INLINE arrL #-}
    arr2A f             = IOSLA $ \ s (x, y) -> runIOSLA (f x) s y
    {-# INLINE arr2A #-}
    constA c            = IOSLA $ \ s   -> const (return (s, [c]))
    {-# INLINE constA #-}
    isA p               = IOSLA $ \ s x -> return (s, if p x then [x] else [])
    {-# INLINE isA #-}
    IOSLA f >>. g       = IOSLA $ \ s x -> do
                                           (s1, ys) <- f s x
                                           return (s1, g ys)
    {-# INLINE (>>.) #-}

    -- just for efficency
    perform (IOSLA f)   = IOSLA $ \ s x -> do
                                           (s1, _ys) <- f s x
                                           return (s1, [x])
    {-# INLINE perform #-}

instance ArrowIf (IOSLA s) where
    ifA (IOSLA p) ta ea = IOSLA $ \ s x -> do
                                           (s1, res) <- p s x
                                           runIOSLA ( if null res
                                                      then ea
                                                      else ta
                                                    ) s1 x

    (IOSLA f) `orElse` g
                        = IOSLA $ \ s x -> do
                                           r@(s1, res) <- f s x
                                           if null res
                                              then runIOSLA g s1 x
                                              else return r


instance ArrowIO (IOSLA s) where
    arrIO cmd           = IOSLA $ \ s x -> do
                                           res <- cmd x
                                           return (s, [res])
    {-# INLINE arrIO #-}

instance ArrowExc (IOSLA s) where
    tryA f              = IOSLA $ \ s x -> do
                                           res <- try' $ runIOSLA f s x
                                           return $ case res of
                                              Left   er      -> (s,  [Left er])
                                              Right (s1, ys) -> (s1, [Right x' | x' <- ys])
        where
        try'            :: IO a -> IO (Either SomeException a)
        try'            = try

instance ArrowIOIf (IOSLA s) where
    isIOA p             = IOSLA $ \ s x -> do
                                           res <- p x
                                           return (s, if res then [x] else [])
    {-# INLINE isIOA #-}

instance ArrowState s (IOSLA s) where
    changeState cf      = IOSLA $ \ s x -> let s' = cf s x in return (seq s' s', [x])
    {-# INLINE changeState #-}
    accessState af      = IOSLA $ \ s x -> return (s, [af s x])
    {-# INLINE accessState #-}

-- ------------------------------------------------------------

-- |
-- lift the state of an IOSLA arrow to a state with an additional component.
--
-- This is uesful, when running predefined IO arrows, e.g. for document input,
-- in a context with a more complex state component.

liftSt          :: IOSLA s1 b c -> IOSLA (s1, s2) b c
liftSt (IOSLA f)
    = IOSLA $ \ (s1, s2) x -> do
                              (s1', ys) <- f s1 x
                              return ((s1', s2), ys)


-- |
-- run an arrow with augmented state in the context of a simple state arrow.
-- An initial value for the new state component is needed.
--
-- This is useful, when running an arrow with an extra environment component, e.g.
-- for namespace handling in XML.

runSt           :: s2 -> IOSLA (s1, s2) b c -> IOSLA s1 b c
runSt s2 (IOSLA f)
    = IOSLA $ \ s1 x -> do
                        ((s1', _s2'), ys) <- f (s1, s2) x
                        return (s1', ys)

-- ------------------------------------------------------------

instance ArrowTree (IOSLA s)

instance (NFData s) => ArrowNF (IOSLA s) where
    rnfA (IOSLA f)      = IOSLA $ \ s x -> do
                                           res <- f s x
                                           deepseq res $ return res

-- ------------------------------------------------------------
