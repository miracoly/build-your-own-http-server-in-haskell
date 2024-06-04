{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Debug.Trace (traceShowId)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Safe (headMay, tailMay)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger (Priority (INFO), infoM, rootLoggerName, setHandlers, setLevel, updateGlobalLogger)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  -- Set up logging to stdout with INFO level using hslogger as simple as possible
  handler <- streamHandler stdout INFO >>= \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger rootLoggerName (setHandlers [handler])
  updateGlobalLogger rootLoggerName (setLevel INFO)
  let logInfo = infoM "Main"

  hSetBuffering stdout LineBuffering

  -- You can use print statements as follows for debugging, they'll be visible when running tests.
  logInfo "Logs from your program will appear here"

  let host = "127.0.0.1"
      port = "4221"

  logInfo $ "Listening on " <> host <> ":" <> port

  -- Get address information for the given host and port
  addrInfo <- getAddrInfo Nothing (Just host) (Just port)

  serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
  setSocketOption serverSocket ReuseAddr 1
  withFdSocket serverSocket setCloseOnExecIfNeeded
  bind serverSocket $ addrAddress $ head addrInfo
  listen serverSocket 5

  logInfo "Ready to accept connections."

  -- Accept connections and handle them forever
  forever $ do
    (clientSocket, clientAddr) <- accept serverSocket
    logInfo $ "Accepted connection from " <> show clientAddr <> "."
    -- Handle the clientSocket as needed...
    rawRequest <- recv clientSocket 1024
    logInfo $ "Request: " <> BC.unpack rawRequest
    case parseRequest rawRequest of
      Nothing -> do
        logInfo "Failed to parse request"
        sendAll clientSocket "HTTP/1.1 400 Bad Request\r\n\r\n"
      Just req -> do
        logInfo $ "Parsed request: " <> show req
        response <- handleRequest req
        logInfo $ "Response: " <> show response
        let serialized = serializeResponse response
        logInfo $ "Sending response: " <> show (BC.unpack serialized)
        sendAll clientSocket serialized

    close clientSocket

handleRequest :: HttpRequest -> IO HttpResponse
handleRequest req = do
  return $ case _reqPath req of
    "/" -> HttpResponse (_reqVersion req) statusOk [("Content-Type", "text/plain")] ""
    "/user-agent" -> mkUserAgentResponse (_reqHeaders req)
    (BC.stripPrefix "/echo/" -> Just str) -> mkEchoResponse str
    _ -> HttpResponse (_reqVersion req) statusNotFound [] ""
data HttpRequest = HttpRequest
  { _reqVersion :: ByteString,
    _reqMethod :: ByteString,
    _reqPath :: ByteString,
    _reqHeaders :: [(ByteString, ByteString)],
    _reqBody :: ByteString
  }
  deriving (Show)

parseRequest :: ByteString -> Maybe HttpRequest
parseRequest raw = do
  version <- getVersion
  path <- getPath
  method <- getMethod
  return $ HttpRequest version method path getHeaders ""
  where
    getMethod = headMay $ BC.split ' ' raw
    getPath = tailMay (BC.split ' ' raw) >>= headMay
    getVersion = tailMay (BC.split ' ' raw) >>= tailMay >>= headMay >>= headMay . BC.split '\r'
    getHeaders = parseHeaders raw

-- TODO
parseHeaders :: ByteString -> [(ByteString, ByteString)]
parseHeaders rawHeaders = []
  where
    bla = splitOn "\r\n" $ BC.unpack rawHeaders

data HttpResponse = HttpResponse
  { _resVersion :: ByteString,
    _resStatus :: HttpStatus,
    _resHeaders :: [(ByteString, ByteString)],
    _resBody :: ByteString
  }
  deriving (Show)

mkUserAgentResponse :: [(ByteString, ByteString)] -> HttpResponse
mkUserAgentResponse reqHeaders =
  let mUserAgent = lookup "User-Agent" reqHeaders
   in case mUserAgent of
        Just userAgent -> HttpResponse "HTTP/0.1" statusOk resHeaders userAgent
          where
            resHeaders = [("Content-Type", "text/plain"), ("Content-Length", (BC.pack . show . BC.length) userAgent)]
        Nothing -> HttpResponse "HTTP/0.1" statusNotFound [] ""

mkEchoResponse :: ByteString -> HttpResponse
mkEchoResponse body = HttpResponse "HTTP/0.1" statusOk headers (traceShowId body)
  where
    headers =
      [ ("Content-Type", "text/plain"),
        ("Content-Length", (BC.pack . show . BC.length) body)
      ]

data HttpStatus = HttpStatus
  { _statusCode :: Int,
    _statusMessage :: ByteString
  }
  deriving (Show)

statusOk :: HttpStatus
statusOk = HttpStatus 200 "OK"

statusNotFound :: HttpStatus
statusNotFound = HttpStatus 404 "Not Found"

serializeResponse :: HttpResponse -> ByteString
serializeResponse res = BC.intercalate "\r\n" [version <> " " <> status, headers, "", body]
  where
    version = _resVersion res
    status = serializeStatus $ _resStatus res
    headers = BC.intercalate "\r\n" $ map (\(k, v) -> k <> ": " <> v) (_resHeaders res)
    body = _resBody res

serializeStatus :: HttpStatus -> ByteString
serializeStatus status = BC.pack (show (_statusCode status)) <> " " <> _statusMessage status