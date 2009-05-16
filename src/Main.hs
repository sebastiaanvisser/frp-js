module Main where

import Compiler
import Demo
import Val
import Network.Salvia.Handler.ExtendedFileSystem
import Network.Salvia.Handlers
import Network.Salvia.Httpd
import Network.Socket

main :: IO ()
main = do
  conf <- defaultConfig
  addr <- inet_addr "127.0.0.1"
  start 
    (conf { listenAddr = addr, listenPort = 8080 })
    (hDefaultEnv root)

root :: Handler ()
root =
    hPath "/www/demo.js" (hFRP demo)
  $ hExtendedFileSystem "."

hFRP :: FRP () -> Handler ()
hFRP = sendStrLn . compile

