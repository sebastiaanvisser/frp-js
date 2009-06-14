module Main where

import Control.Concurrent
import Control.Monad.State
import Core.Compiler
import Core.Dot
import Core.Val
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Handler.ExtendedFileSystem
import Network.Salvia.Handlers
import Network.Salvia.Httpd
import Network.Socket
import System.IO
import System.Process
import qualified Demo1 as Demo1
import qualified Demo2 as Demo2

main :: IO ()
main = do
  conf <- defaultConfig
  addr <- inet_addr "127.0.0.1"
  start 
    (conf { listenAddr = addr, listenPort = 8080 })
    (hDefaultEnv root)

root :: Handler ()
root = hPathRouter
  [ ("/www/demo1/demo.js",  hFrp Demo1.demo)
  , ("/www/demo1/demo.png", hDot Demo1.demo)
  , ("/www/demo2/demo.js",  hFrp Demo2.demo)
  , ("/www/demo2/demo.png", hDot Demo2.demo)
  ] $ hExtendedFileSystem "."

hFrp :: FRP () -> Handler ()
hFrp s = liftIO (compiler s) >>= sendStrLn

hDot :: FRP () -> Handler ()
hDot frp = 
  do dot <- liftIO $ render frp
     enterM response $ do
       setM contentType ("image/png", Nothing)
       setM status OK
     (i, o, _, _) <- liftIO (runInteractiveProcess "dot" ["-Tpng"] Nothing Nothing)
     liftIO $ forkIO (hPutStrLn i dot >> hClose i)
     spoolBs id o

