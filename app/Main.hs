{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception (IOException, catch)
import Control.Monad (forever)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT), ask)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Options.Applicative (Parser, auto, execParser, help, info, long, metavar, option, short, strOption, value)
import Safe (headMay, tailMay)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger (Priority (INFO), errorM, infoM, rootLoggerName, setHandlers, setLevel, updateGlobalLogger)

data Config = Config
  { _directory :: FilePath,
    _port :: Int
  }
  deriving (Show, Eq)

argsParser :: Parser Config
argsParser =
  Config
    <$> strOption
      ( long "directory"
          <> short 'd'
          <> metavar "DIRECTORY"
          <> value "./"
          <> help "Directory to serve files from"
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value 4221
          <> help "Port to listen on"
      )

type App = ReaderT Config IO

main :: IO ()
main = do
  handler <- streamHandler stdout INFO >>= \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger rootLoggerName (setHandlers [handler])
  updateGlobalLogger rootLoggerName (setLevel INFO)

  config <- execParser $ info argsParser mempty
  logInfo $ "Configuration: " <> show config

  hSetBuffering stdout LineBuffering

  -- You can use print statements as follows for debugging, they'll be visible when running tests.
  logInfo "Logs from your program will appear here"

  let host = "127.0.0.1"
      port = (show . _port) config

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
    forkIO $ runReaderT (handleRequest clientSocket) config

logInfo :: String -> IO ()
logInfo = infoM "Main"

logError :: String -> IO ()
logError = errorM "Main"

handleRequest :: Socket -> App ()
handleRequest clientSocket = do
  config <- ask
  liftIO $ do
    -- Handle the clientSocket as needed...
    rawRequest <- recv clientSocket 4096
    logInfo $ "Request: " <> BC.unpack rawRequest
    case parseRequest rawRequest of
      Nothing -> do
        logInfo "Failed to parse request"
        sendAll clientSocket "HTTP/1.1 400 Bad Request\r\n\r\n"
      Just req -> do
        logInfo $ "Parsed request: " <> show req
        response <- runReaderT (respond req) config
        logInfo $ "Response: " <> show response
        let serialized = serializeResponse response
        logInfo $ "Sending response: " <> show (BC.unpack serialized)
        sendAll clientSocket serialized
    close clientSocket

respond :: HttpRequest -> App HttpResponse
respond req =
  case _reqMethod req of
    "GET" -> respondGet req
    "POST" -> respondPost req
    _ -> return $ notFoundResponse req

respondGet :: HttpRequest -> App HttpResponse
respondGet req =
  let
    acceptsGzip :: Bool
    acceptsGzip =  ((Just "gzip" ==) . lookup "Accept-Encoding") $ _reqHeaders req
   in case _reqPath req of
        "/" -> return $ HttpResponse (_reqVersion req) statusOk [("Content-Type", "text/plain")] ""
        "/user-agent" -> return $ mkUserAgentResponse (_reqHeaders req)
        (BC.stripPrefix "/echo/" -> Just str) -> return $ mkEchoResponse acceptsGzip str
        (BC.stripPrefix "/files/" -> Just fileName) -> mkGetFileResponse $ BC.unpack fileName
        _ -> return $ notFoundResponse req

respondPost :: HttpRequest -> App HttpResponse
respondPost req =
  case _reqPath req of
    (BC.stripPrefix "/files/" -> Just fileName) -> mkPostFileResponse (BC.unpack fileName) (_reqBody req)
    _ -> return $ notFoundResponse req

notFoundResponse :: HttpRequest -> HttpResponse
notFoundResponse req = HttpResponse (_reqVersion req) statusNotFound [] ""

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
  return $ HttpRequest version method path getHeaders getBody
  where
    getMethod = headMay $ BC.split ' ' raw
    getPath = tailMay (BC.split ' ' raw) >>= headMay
    getVersion = tailMay (BC.split ' ' raw) >>= tailMay >>= headMay >>= headMay . BC.split '\r'
    getHeaders = parseHeaders raw
    getBody :: ByteString
    getBody = (BC.pack . last . splitOn "\r\n" . BC.unpack) raw

parseHeaders :: ByteString -> [(ByteString, ByteString)]
parseHeaders = parse . split
  where
    parse :: [String] -> [(ByteString, ByteString)]
    parse = map (mapTupl2 BC.pack) . mapMaybe (toTuple . splitOn ": ")
    split :: ByteString -> [String]
    split = takeWhile (/= "\r\n") . drop 1 . splitOn "\r\n" . BC.unpack
    mapTupl2 :: (a -> b) -> (a, a) -> (b, b)
    mapTupl2 f (k, v) = (f k, f v)
    toTuple :: [String] -> Maybe (String, String)
    toTuple ls =
      case ls of
        (k : v : _) -> Just (k, v)
        _ -> Nothing

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
        Just userAgent -> HttpResponse "HTTP/1.1" statusOk resHeaders userAgent
          where
            resHeaders = [("Content-Type", "text/plain"), ("Content-Length", (BC.pack . show . BC.length) userAgent)]
        Nothing -> HttpResponse "HTTP/1.1" statusNotFound [] ""

mkEchoResponse :: Bool -> ByteString -> HttpResponse
mkEchoResponse encodeGzip body = HttpResponse "HTTP/1.1" statusOk headers body
  where
    headers =
      [ ("Content-Type", "text/plain"),
        ("Content-Length", (BC.pack . show . BC.length) body)
      ] <> ([("Content-Encoding", "gzip") | encodeGzip])


mkGetFileResponse :: FilePath -> App HttpResponse
mkGetFileResponse filename = do
  config <- ask
  let path = _directory config <> filename
  liftIO $ catch (successResponse path) errorResponse
  where
    readContent = fmap BC.pack . readFile
    successResponse :: FilePath -> IO HttpResponse
    successResponse path = do
      fileContent <- readContent path
      return $ HttpResponse "HTTP/1.1" statusOk (headers fileContent) fileContent
    errorResponse :: IOException -> IO HttpResponse
    errorResponse e = do
      logError $ "Cannot read file: " <> show e
      return $ HttpResponse "HTTP/1.1" statusNotFound [] ""
    headers content =
      [ ("Content-Type", "application/octet-stream"),
        ("Content-Length", (BC.pack . show . BC.length) content)
      ]

mkPostFileResponse :: FilePath -> ByteString -> App HttpResponse
mkPostFileResponse filename content = do
  config <- ask
  let path = _directory config <> filename
  liftIO $ catch (successResponse path) errorResponse
  where
    successResponse :: FilePath -> IO HttpResponse
    successResponse path = do
      writeFile path $ BC.unpack content
      return $ HttpResponse "HTTP/1.1" statusCreated [] ""
    errorResponse :: IOException -> IO HttpResponse
    errorResponse e = do
      logError $ "Cannot write file: " <> show e
      return $ HttpResponse "HTTP/1.1" statusInternalServerError [] ""

data HttpStatus = HttpStatus
  { _statusCode :: Int,
    _statusMessage :: ByteString
  }
  deriving (Show)

statusOk :: HttpStatus
statusOk = HttpStatus 200 "OK"

statusCreated :: HttpStatus
statusCreated = HttpStatus 201 "Created"

statusNotFound :: HttpStatus
statusNotFound = HttpStatus 404 "Not Found"

statusInternalServerError :: HttpStatus
statusInternalServerError = HttpStatus 500 "Internal Server Error"

serializeResponse :: HttpResponse -> ByteString
serializeResponse res = BC.intercalate "\r\n" [version <> " " <> status, headers, "", body]
  where
    version = _resVersion res
    status = serializeStatus $ _resStatus res
    headers = BC.intercalate "\r\n" $ map (\(k, v) -> k <> ": " <> v) (_resHeaders res)
    body = _resBody res

serializeStatus :: HttpStatus -> ByteString
serializeStatus status = BC.pack (show (_statusCode status)) <> " " <> _statusMessage status