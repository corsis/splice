{-# LANGUAGE CPP #-}
-- | Module      : Network.Socket.Splice
-- Copyright   : (c) Cetin Sert 2012
-- License     : BSD3
-- Maintainer  : fusion@corsis.eu
-- Stability   : Stable
-- Portability : Portable
--
-- 'splice' is the cross-platform API for continous, uni-directional
-- data transfer between two network sockets.
--
-- 'splice' and its implementation primitives 'hSplice' and 'fdSplice' are
-- /infinite/ loops that are intended to be used with
-- 'Control.Concurrent.forkIO' and exception handlers. 'splice' is a
-- terminal operation; it cannot be interleaved by other IO operations
-- on its sockets or handles.
--
-- [Initiate bi-directional continuous data transfer between two sockets:]
--
-- > void . forkIO . tryWith handler $! splice void 1024 (sourceSocket, _) (targetSocket, _)
-- > void . forkIO . tryWith handler $! splice void 1024 (targetSocket, _) (sourceSocket, _)
--
-- where @handler@ is an IO operation that would do the necessary clean up –
-- such as ensuring the sockets are closed and any resources that may be
-- associated with the sockets are properly disposed of.
--
-- [Notes]
--
--   * 'System.IO.Splice.Linux.c_splice', the Linux-only system call, is not
--     a terminal infinite loop and can be safely interleaved by other IO
--     operations on sockets or socket handles.
module Network.Socket.Splice (
  -- * Cross-platform interface
    splice
  , usingSplice

  -- * Combinators for Exception Handling
  , tryWith
  , try_
  ) where

import Prelude hiding (sin)
import Network.Socket
import Control.Monad (void)
import Control.Exception

#ifdef LINUX_SPLICE
import qualified System.IO.Splice.Linux as I
#else
import qualified System.IO.Splice.Portable as I
#endif

----------------------------------------------------------------------------------------------SPLICE


-- | Pipes data from one socket to another in an /infinite loop/.
--
--   'splice' currently has two implementations:
--
--   [on GNU\/Linux using 'fdSplice' ≅]
--
--   > splice len (sIn, _       ) (sOut, _        ) = ... fdSplice ...
--
--   [on all other operating systems using 'hSplice' ≅]
--
--   > splice len (_  , Just hIn) (_   , Just hOut) = ... hSplice  ...
--
--   [Notes]
--
--     * 'fdSplice' and 'fdSplice' implementation of 'splice' are only available
--        on GNU\/Linux.
--
--     * 'hSplice' is always available and the 'hSplice' implementation of
--       'splice' can be forced on GNU\/Linux by defining the @portable@ flag at
--       compile time.
--
--     * 'hSplice' implementation requires handles in 'NoBuffering' mode.
--
--     * 'splice' is a terminal loop on two sockets and once entered its sockets
--        and handles cannot be interleaved by other IO operations.
--
splice :: Integer   -- ^ Maximal chunk size
       -> Socket    -- ^ Source socket
       -> Socket    -- ^ Target socket
       -> IO ()
splice sz = I.spliceLoop (fromIntegral sz)

-- | Indicates whether 'splice' uses zero-copy system calls or the portable user
--   space Haskell implementation.
usingSplice :: Bool -- ^ @True@ if 'splice' uses zero-copy system calls; otherwise, false.
usingSplice =
#ifdef LINUX_SPLICE
  True
#else
  False
#endif

------------------------------------------------------------------------------------------EXCEPTIONS

-- | Similar to 'Control.Exception.Base.try' but used when an obvious exception
--   is expected and can be handled easily. Unlike 'finally' exceptions are
--   /NOT/ rethrown once handled.
tryWith
  :: (SomeException -> IO a) -- ^ exception handler.
  -> IO a                    -- ^ action to run which can throw /any/ exception.
  -> IO a                    -- ^ new action where all exceptions are handled by the single handler.
tryWith h a = try a >>= \r -> case r of Left x -> h x; Right y -> return y


-- | Similar to 'Control.Exception.Base.try' but used when an obvious exception
--   is expected which can be safely ignored.
try_
  :: IO ()                   -- ^ action to run which can throw /any/ exception.
  -> IO ()                   -- ^ new action where exceptions are silenced.
try_ a = void (try a :: IO (Either SomeException ()))
