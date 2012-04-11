-- | Implementation.
-- Module      : Network.Socket.Splice.Internal
-- Copyright   : (c) Cetin Sert 2012
-- License     : BSD3
-- Maintainer  : fusion@corsis.eu
-- Stability   : stable
-- Portability : GHC-only, works on all operating systems


#ifdef LINUX_SPLICE
#include <fcntl.h>
#endif
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}


module Network.Socket.Splice.Internal (

  -- * Cross-platform API for Socket to Socket Data Transfer Loops
  {- | 'splice' is the cross-platform API for continous, uni-directional
       data transfer between two network sockets.

       It is an /infinite loop/ that is intended to be used with
       'Control.Concurrent.forkIO':

       > void . forkIO . try_ $ splice 1024 sourceSocket targetSocket
       > void . forkIO . try_ $ splice 1024 targetSocket sourceSocket
  -}

    splice
  , ChunkSize
  , zeroCopy

  -- * Combinators for Exception Handling
  , try_

  ) where


import Data.Word
import Foreign.Ptr

import Network.Socket
import Control.Monad
import Control.Exception

#ifdef LINUX_SPLICE
import Data.Int
import Data.Bits
import System.Posix.IO
import Unsafe.Coerce
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Internals
import qualified System.IO.Splice.Linux as L
#else
import System.IO
import GHC.IO.Handle.FD
import Foreign.Marshal.Alloc
#endif


--------------------------------------------------------------------------------


-- | Indicates whether 'splice' uses zero-copy system calls or the portable user 
--   space Haskell implementation.
zeroCopy :: Bool -- ^ @True@ if 'splice' uses zero-copy system calls;
                 --   otherwise, false.
zeroCopy =
#ifdef LINUX_SPLICE
  True
#else
  False
#endif


-- | The numeric type to recommend chunk sizes for moving data between sockets
--   used by both zero-copy and portable implementations of 'splice'.
type ChunkSize =
#ifdef LINUX_SPLICE
  L.ChunkSize
#else
  Int
#endif


-- | Pipes data from one socket to another in an /infinite loop/.
--
--   On Linux this uses the @splice()@ system call and eliminates copying
--   between kernel and user address spaces.
--
--   On other operating systems, a portable Haskell implementation utilizes a
--   user space buffer.
splice
  :: ChunkSize -- ^ chunk size.
  -> Socket    -- ^ source socket.
  -> Socket    -- ^ target socket.
  -> IO ()     -- ^ infinite loop.
splice len sIn sOut = do

  let throwRecv0 = error "Network.Socket.Splice.splice ended"

  let fdIn  = fdSocket sIn
  let fdOut = fdSocket sOut 

#ifdef LINUX_SPLICE

  (r,w) <- createPipe     -- r,w: read / write ends of pipe
  let s = Fd fdIn         -- s  : source socket
  let t = Fd fdOut        -- t  : target socket
  let n = nullPtr  
  let u = unsafeCoerce :: (#type ssize_t) -> (#type size_t)
  let check = throwErrnoIfMinus1 "Network.Socket.Splice.splice"
  let flags = L.sPLICE_F_MOVE .|. L.sPLICE_F_MORE
  let setNonBlockingMode v = do setNonBlockingFD fdIn  v
                                setNonBlockingFD fdOut v

  setNonBlockingMode False
  finally
    (forever $ do 
       bytes <- check $ L.c_splice s n w n    len    flags
       if bytes > 0
         then           L.c_splice r n t n (u bytes) flags
         else           throwRecv0)
    (do closeFd r
        closeFd w
        try_ $ setNonBlockingMode True)

#else

  s <- fdToHandle fdIn ; hSetBuffering s NoBuffering
  t <- fdToHandle fdOut; hSetBuffering t NoBuffering
  a <- mallocBytes len :: IO (Ptr Word8)

  finally
    (forever $ do
       bytes <-   hGetBufSome s a len
       if bytes > 0
         then     hPutBuf     t a bytes
         else     throwRecv0)
    (do free a
        try_ $ hClose s
        try_ $ hClose t)

#endif


-- | Similar to 'Control.Exception.Base.try' but used when an obvious exception
--   is expected which can be safely ignored.
try_
  :: IO () -- ^ action to run which can throw /any/ exception.
  -> IO () -- ^ new action where exceptions are silenced.
try_ a = (try a :: IO (Either SomeException ())) >> return ()
