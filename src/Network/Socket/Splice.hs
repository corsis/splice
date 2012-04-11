{- |
  This library implements most efficient socket to socket data transfer loops
  for proxy servers on each operating system.
               
  On Linux, it uses and exposes the zero-copy @splice()@ system call:
  <http://kerneltrap.org/node/6505>.

  On other operating systems, it currently falls back to a portable Haskell
  implementation – which first allocates a constant-sized memory buffer in user
  address, then enters an inner loop which uses 'System.IO.hGetBufSome'
  and 'System.IO.hPutBuf' on this user-space buffer. This avoids tony of tiny
  allocations as might otherwise be caused by 'Network.Socket.ByteString.recv'
  and 'Network.Socket.ByteString.sendAll' from the @bytestring@ package.
-}
--  
-- Module      : Network.Socket.Splice
-- Copyright   : (c) Cetin Sert 2012
-- License     : BSD3
-- Maintainer  : fusion@corsis.eu
-- Stability   : stable
-- Portability : GHC-only, works on all OSes

module Network.Socket.Splice (

  module Network.Socket.Splice.Internal

  ) where

import Network.Socket.Splice.Internal
