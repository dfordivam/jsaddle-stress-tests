{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backend where

import Common.Route
import Obelisk.Backend

import Data.Text
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Dependent.Map (DSum (..))
import Network.WebSockets
import Network.WebSockets.Snap

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      liftIO $ serve $ \case
        BackendRoute_Missing :=> _ -> pure ()
        BackendRoute_WebSocket :=> _ -> runWebSocketsSnap $ \pending -> do
          liftIO $ putStrLn "Starting websocket"
          conn <- acceptRequest pending
          limitTxt :: Text <- receiveData conn
          let (limit, delay) = read $ unpack limitTxt
          forM_ [0..limit] $ \i -> do
            threadDelay delay
            sendTextData conn (pack $ show i)
  , _backend_routeEncoder = fullRouteEncoder
  }
