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
-- 'splice' and its implementation primitives are
-- /infinite/ loops that are intended to be used with
-- 'Control.Concurrent.forkIO' and exception handlers. 'splice' is a
-- terminal operation; it cannot be interleaved by other IO operations
-- on its sockets or handles.
--
-- [Initiate bi-directional continuous data transfer between two sockets:]
--
-- > void . forkIO . tryWith handler $! splice 1024 sourceSocket targetSocket
-- > void . forkIO . tryWith handler $! splice 1024 targetSocket sourceSocket
--
-- where @handler@ is an IO operation that would do the necessary clean up –
-- such as ensuring the sockets are closed and any resources that may be
-- associated with the sockets are properly disposed of.
--
module Network.Socket.Splice (
  -- * Cross-platform interface
    splice
  , usingSplice2

  -- * Combinators for Exception Handling
  , tryWith
  , try_
  ) where

import Prelude hiding (sin)
import Network.Socket

import System.IO.Splice.Util

#ifdef LINUX_SPLICE
import qualified System.IO.Splice.Linux as I
#else
import qualified System.IO.Splice.Portable as I
#endif

----------------------------------------------------------------------------------------------SPLICE


-- | Pipes data from one socket to another in an /infinite loop/. If
-- available, it will use the @splice(2)@ system call to allow zero-copy
-- transfer between sockets. Otherwise it will use a portable Haskell
-- implementation.
--
--   [Notes]
--
--     * 'splice' is a terminal loop on two sockets and once entered its sockets
--       and handles cannot be interleaved by other IO operations.
--
--     * As a corollary, you obviously should not use the source/target
--       sockets again
--
--     * The @Socket@s will be closed when if an exception is thrown,
--       terminating the loop.
splice :: Integer   -- ^ Maximal chunk size
       -> Socket    -- ^ Source socket
       -> Socket    -- ^ Target socket
       -> IO ()     -- ^ Infinite loop
splice sz = I.spliceLoop (fromIntegral sz)

-- | Indicates whether 'splice' uses zero-copy system @splice(2)@
-- system call, or the portable user space Haskell implementation.
usingSplice2 :: Bool -- ^ @True@ if 'splice' uses zero-copy system calls; otherwise, false.
usingSplice2 =
#ifdef LINUX_SPLICE
  True
#else
  False
#endif
