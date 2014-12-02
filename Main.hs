{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.MVar
import           Control.Exception              (fromException, handle)
import           Control.Monad
-- import           Data.List                      (delete)
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

type AppState = MVar [WS.Connection]

main :: IO ()
main = do
  state <- newMVar []
  run 3001 (app state)

app :: AppState -> Application
app state = static $ WaiWS.websocketsOr WS.defaultConnectionOptions (wsApp state) (app' state)

wsApp :: AppState -> WS.ServerApp
wsApp state pending = do
    log $ "Will do WS.acceptRequest"
    conn <- WS.acceptRequest pending
    log $ "Websocket client connected."

    log $ "Putting conn to list of connections"
    modifyMVar_ state (\l -> return (conn:l))
    l <- readMVar state
    log $ "length l: " ++ show (length l)

    log $ "Doing receiveData."
    msg <- WS.receiveData conn :: IO Text
    log $ "msg: " <> T.unpack msg

    -- notify everybody known
    let msg' = "BROADCASTED MESSAGE FROM wsApp. Length is: "
                 <> T.pack (show (length l)) :: Text
    broadcast state msg'

    talk conn
  where
    talk :: WS.Connection -> IO ()
    talk conn = handle catchDisconnect $
        forever $ do
            msg <- WS.receiveData conn
            broadcast state ("Someone sent a message: " <> msg)
      where
        catchDisconnect e = case fromException e of
            Just WS.ConnectionClosed -> do
                log $ "Should disconnect someone (ideally)"
                broadcast state "Someone disconnected"
                removeClient conn state
            _ -> return ()

removeClient :: WS.Connection -> AppState -> IO ()
removeClient _ _ = return ()
  -- modifyMVar_ state $ \l -> return (delete conn l)

broadcast :: AppState -> Text -> IO ()
broadcast s msg = do
    l <- readMVar s
    forM_ l $ \conn -> do
        log $ "Sending broadcast"
        WS.sendTextData conn msg

app' :: AppState -> Application
app' state req respond = do
    -- broadcast on post
    when (requestMethod req == "POST") $ do
        log $ "Got POST request. Will send message to all clients"
        l <- readMVar state
        log $ "length l: " ++ show (length l)
        let msg = "BROADCASTED MESSAGE FROM wsApp. Length is: "
                    <> T.pack (show (length l)) :: Text
        broadcast state msg
    respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

log :: String -> IO ()
log = putStrLn
