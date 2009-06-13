module Main where

import Control.Monad.State
import Control.Concurrent
import Core.Compiler
import Core.NewCompiler
import Core.Dot
import Core.Val
import Data.Record.Label
import Demo
import Network.Protocol.Http
import Network.Salvia.Handler.ExtendedFileSystem
import Network.Salvia.Handlers
import Network.Salvia.Httpd
import Network.Socket
import System.IO
import System.Process

main :: IO ()
main = do
  conf <- defaultConfig
  addr <- inet_addr "127.0.0.1"
  start 
    (conf { listenAddr = addr, listenPort = 8080 })
    (hDefaultEnv root)

root :: Handler ()
root =
    hPath "/www/demo.js"     (hFrp2 demo)
  $ hPath "/www/demo.png"    (hDot demo)
  $ hExtendedFileSystem "."

hFrp :: FRP () -> Handler ()
hFrp = sendStrLn . compile

hFrp2 :: FRP () -> Handler ()
hFrp2 s = liftIO (compiler1 s) >>= sendStrLn

hDot :: FRP () -> Handler ()
hDot frp = 
  do dot <- liftIO $ render frp
     enterM response $ do
       setM contentType ("image/png", Nothing)
       setM status OK
     (i, o, _, _) <- liftIO (runInteractiveProcess "dot" ["-Tpng"] Nothing Nothing)
     lift $ forkIO (hPutStrLn i dot >> hClose i)
     spoolBs id o

