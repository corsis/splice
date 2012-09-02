-- |
-- Module      : System.IO.Splice.Util
-- Copyright   : (c) Cetin Sert 2012
-- License     : BSD3
-- Maintainer  : fusion@corsis.eu
-- Stability   : Stable
-- Portability : Portable
--
-- Exposes a portable alternative to the `splice(2)` system call
-- on non-GNU\/Linux machines.
--
module System.IO.Splice.Util
       ( try_
       , tryWith
       , throwRecv0
       ) where

import Control.Monad
import Control.Exception

-- | Similar to 'Control.Exception.Base.try' but used when an obvious exception
-- is expected and can be handled easily. Unlike 'finally' exceptions are
-- /NOT/ rethrown once handled.
tryWith :: (SomeException -> IO a) -- ^ exception handler.
        -> IO a                    -- ^ action to run which can throw /any/ exception.
        -> IO a                    -- ^ new action where all exceptions are handled by the single handler.
tryWith h a = try a >>= \r -> case r of Left x -> h x; Right y -> return y


-- | Similar to 'Control.Exception.Base.try' but used when an obvious exception
-- is expected which can be safely ignored.
try_ :: IO ()                   -- ^ action to run which can throw /any/ exception.
     -> IO ()                   -- ^ new action where exceptions are silenced.
try_ a = void (try a :: IO (Either SomeException ()))

-- | Throws a receive error for when we recieve 0 bytes,
-- i.e. our sockets die.
throwRecv0 :: a
throwRecv0 = error "System.IO.Splice.Util: spliceLoop ended"
