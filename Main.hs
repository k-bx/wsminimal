{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Network.HTTP.Types             (status200)
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import           Network.Wai.Middleware.Static
import qualified Network.WebSockets             as WS
import           Prelude                        hiding (log)

main :: IO ()
main = run 3001 app

app :: Application
app = static $ WaiWS.websocketsOr WS.defaultConnectionOptions wsApp app'

wsApp :: WS.ServerApp
wsApp pending = do
    log $ "Will do WS.acceptRequest"
    conn <- WS.acceptRequest pending
    log $ "Websocket client connected."
    log $ "Doing receiveData."
    msg <- WS.receiveData conn :: IO Text
    log $ "msg: " <> T.unpack msg
    return ()

app' :: Application
app' req respond = respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

log :: String -> IO ()
log = putStrLn
