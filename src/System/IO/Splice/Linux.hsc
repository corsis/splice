{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- |
-- Module      : System.IO.Splice.Linux
-- Copyright   : (c) Cetin Sert 2012
-- License     : BSD3
-- Maintainer  : fusion@corsis.eu
-- Stability   : Stable
-- Portability : GNU/Linux only
--
-- Exposes the GNU\/Linux @splice(2)@ system call: <http://kerneltrap.org/node/6505>
--
module System.IO.Splice.Linux (
    -- * Types
    ChunkSize
    -- * System call loop
  , spliceLoop
    -- * FFI bindings
  , c_splice
  , sPLICE_F_MOVE
  , sPLICE_F_MORE
  , sPLICE_F_NONBLOCK
  ) where

import Data.Int
import Data.Word
import Data.Bits

import Network.Socket
import Control.Monad
import Control.Exception

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.Error

import System.Posix.IO
import System.Posix.Types

#include <fcntl.h>

-- | Chunk size for moving data between sockets.
type ChunkSize = (#type size_t)

{- | GNU\/Linux @splice()@ system call loop.

       1. creates a pipe in kernel address space

       2. uses it until the loop terminates by exception

       3. closes the pipe and returns
-}
spliceLoop :: ChunkSize -> Socket -> Socket -> IO ()
spliceLoop len inp outp = do
  let s = Fd (fdSocket inp)
      t = Fd (fdSocket outp)
  (r,w) <- createPipe
  let flags = sPLICE_F_MOVE .|. sPLICE_F_MORE

  -- Should only be used to check c_splice calls
  let check res | res ==  0 = throwRecv0   -- Nothing left
                | res /= -1 = return False -- OK transfer
                | otherwise = do           -- \*Possible\* error
                    err <- getErrno
                    if (err == eAGAIN) then return True
                     else throwErrno "spliceLoop"

  -- Simple loop to check for EAGAIN
  let loop act = do
        res <- act
        again <- check res
        if again then loop act else return res

  -- Now we loop forever, splicing the sockets together
  finally
    (forever $ do
       b <- loop $ c_splice s nullPtr w nullPtr len flags
       void $ loop $ c_splice r nullPtr t nullPtr (fromIntegral b) flags
    )
    (closeFd r >> closeFd w >> closeFd s >> closeFd t)

--
-- FFI
--

-- | Moves data between two file descriptors without copying between kernel
--   address space and user address space. It transfers up to @len@ bytes of
--   data from the file descriptor @fd_in@ to the file descriptor @fd_out@,
--   where one of the descriptors must refer to a pipe.
--
--   'c_splice' is /NOT/ a loop and needs to be called repeatedly.
--
--   For an example, see 'Network.Socket.Splice.Internal.splice'.
foreign import ccall "splice"
  c_splice
  :: Fd                  -- ^ @fd_in@.
  -> Ptr (#type loff_t)  -- ^ @off_in@.
  -> Fd                  -- ^ @fd_out@.
  -> Ptr (#type loff_t)  -- ^ @off_out@.
  -> ChunkSize           -- ^ @len@.
  -> Word                -- ^ @flags@.
  -> IO (#type ssize_t)  -- ^ number of bytes moved if successful; otherwise -1.


-- | Attempt to move pages instead of copying. This is only a hint to the
--   kernel: pages may stil be copied (/in kernel address space/) if the kernel
--   cannot move the pages from the pipe, or if the pipe buffers don't refer to
--   full pages.
sPLICE_F_MOVE :: Word
sPLICE_F_MOVE = (#const "SPLICE_F_MOVE")


-- | More data will be coming in a subsequent 'c_splice'. This is a helpful hint
--   when @fd_out@ refers to a socket.
sPLICE_F_MORE :: Word
sPLICE_F_MORE = (#const "SPLICE_F_MORE")


-- | Do not block on I\/O. This makes the 'c_splice' pipe operations
--   nonblocking, but 'c_splice' may nevertheless block because the file
--   descriptors that are 'c_splice'd to\/from may block (unless they have the
--   @O_NONBLOCK@ flag set).
sPLICE_F_NONBLOCK :: Word
sPLICE_F_NONBLOCK = (#const "SPLICE_F_NONBLOCK")

throwRecv0 :: a
throwRecv0 = error "System.IO.Splice.Linux.spliceLoop ended"
