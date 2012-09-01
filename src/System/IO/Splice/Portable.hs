-- |
-- Module      : System.IO.Splice.Portable
-- Copyright   : (c) Cetin Cert 2012
-- License     : BSD3
-- Maintainer  : fusion@corsis.eu
-- Stability   : Stable
-- Portability : Portable
--
-- Exposes a portable alternative to the `splice(2)` system call
-- on non-GNU\/Linux machines.
--
module System.IO.Splice.Portable
       ( -- * Types
         ChunkSize
         -- * System call loop
       , spliceLoop
       ) where

import Data.Word
import System.IO
import Network.Socket

import Control.Monad
import Control.Exception

import Foreign.Ptr
import Foreign.Marshal.Alloc

-- | Chunk size for moving data between sockets.
type ChunkSize = Int

{- | The portable Haskell loop.

       1. allocates a /single/ memory buffer in user address space

       2. uses it until the loop terminates by exception

       3. frees the buffer and returns

   [Notes]

     * the socket handles are required to be in 'NoBuffering' mode.
-}
spliceLoop :: Int -> Socket -> Socket -> IO ()
spliceLoop len inp outp = do
  s <- socketToHandle inp ReadWriteMode
  t <- socketToHandle outp ReadWriteMode
  a <- mallocBytes len :: IO (Ptr Word8)
  finally
    (forever $! do
       bytes   <- hGetBufSome s a len
       if bytes > 0
         then     hPutBuf     t a bytes
         else     throwRecv0)
    (free a)

throwRecv0 :: a
throwRecv0 = error "System.IO.Splice.Portable.spliceLoop ended"
