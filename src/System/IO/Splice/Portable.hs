-- |
-- Module      : System.IO.Splice.Portable
-- Copyright   : (c) Cetin Sert 2012
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

import Data.ByteString as S
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (send, recv)

import Control.Monad
import Control.Exception


-- | Chunk size for moving data between sockets.
type ChunkSize = Int

-- | The portable Haskell loop.
spliceLoop :: Int -> Socket -> Socket -> IO ()
spliceLoop len inp outp = do
  let sClose' = try_ . sClose
  finally
    (forever $! do
       bs <- recv inp len
       if S.length bs > 0
         then send outp bs
         else throwRecv0)
    (sClose' inp >> sClose' outp)

try_ :: IO () -> IO ()
try_ a = void (try a :: IO (Either SomeException ()))

throwRecv0 :: a
throwRecv0 = error "System.IO.Splice.Portable.spliceLoop ended"
