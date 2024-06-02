{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Network.Socket.ByteString (sendAll)
import System.Log.Logger (rootLoggerName, setLevel, updateGlobalLogger, Priority (INFO), setHandlers, infoM, getLogger)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Handler (setFormatter)
import System.Log.Formatter (simpleLogFormatter)

main :: IO ()
main = do
  -- Set up logging to stdout with INFO level using hslogger as simple as possible
  handler <- streamHandler stdout INFO >>= \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger rootLoggerName (setHandlers [handler])
  updateGlobalLogger rootLoggerName (setLevel INFO)
  let log = infoM "Main"

  hSetBuffering stdout LineBuffering

  -- You can use print statements as follows for debugging, they'll be visible when running tests.
  log "Logs from your program will appear here"

  let host = "127.0.0.1"
      port = "4221"

  log $ "Listening on " <> host <> ":" <> port

  -- Get address information for the given host and port
  addrInfo <- getAddrInfo Nothing (Just host) (Just port)

  serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
  bind serverSocket $ addrAddress $ head addrInfo
  listen serverSocket 5

  log "Ready to accept connections."

  -- Accept connections and handle them forever
  forever $ do
    (clientSocket, clientAddr) <- accept serverSocket
    log $ "Accepted connection from " <> show clientAddr <> "."
    -- Handle the clientSocket as needed...
    sendAll clientSocket "HTTP/1.1 200 OK\r\n\r\n"

    close clientSocket
