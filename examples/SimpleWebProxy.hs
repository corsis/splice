module Main
       ( main -- :: IO ()
       ) where

import Control.Monad
import Control.Concurrent
import System.Environment

import Network.Socket
import Network.Socket.Splice

main :: IO ()
main = withSocketsDo $ do
  [h',p'] <- getArgs
  let p = fromIntegral (read p') :: PortNumber
  h <- inet_addr h'

  serve <- serverInit
  putStrLn "Server initialized on localhost:8080..."
  forever $
    accept serve >>= forkIO . client h p

client host port (sock,_) = do
    putStrLn "Serving client..."
    prox <- newSock
    connect prox (SockAddrInet port host)
    putStrLn "Connected to other end"
    putStrLn "Splicing connections together"
    splicer sock prox

serverInit :: IO Socket
serverInit = do
  sock <- newSock
  bindSocket sock (SockAddrInet 8080 iNADDR_ANY)
  listen sock 2
  return sock

newSock :: IO Socket
newSock = socket AF_INET Stream defaultProtocol

splicer :: Socket -> Socket -> IO ()
splicer s t = do
    void $ forkIO $ try_ (splice 1024 s t)
    void $ forkIO $ try_ (splice 1024 t s)
