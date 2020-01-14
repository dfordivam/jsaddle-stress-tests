{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Reflex.Dom.Core

import Common.Api
import Common.Route

import Safe
import Control.Applicative
import Data.Int
import Data.Time
import Control.Monad.IO.Class
import System.Random
import qualified GHCJS.DOM.DOMRectReadOnly as DOM
import qualified GHCJS.DOM.Element as DOM

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      prerender (pure ()) $ do
        throughputTest
        websocketMsgThroughput
      return ()
  }

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

readInt :: T.Text -> Maybe Int
readInt = readMay . T.unpack

showTime lbl = el "p" $ do
  t <- liftIO getCurrentTime
  text (lbl <> " : " <> tshow t)

throughputTest :: (_) => m ()
throughputTest = do
  el "h3" $ text "DOM API call throughput test"
  startEv <- do
    el "p" $ text "Enter number of requests to do"
    v <- value <$> textInput (def & textInputConfig_initialValue .~ "10000")
    ev <- button "Start"
    let vInt = tag (current $ readInt <$> v) ev
    pure (fmapMaybe id vInt)

  let showTime lbl = el "p" $ do
        t <- liftIO getCurrentTime
        text (lbl <> " : " <> tshow t)
  widgetHold_ blank $ ffor startEv $ \limit -> mdo
    el "p" $ do
      let attr = ffor cnt $ \v -> ("max" =: tshow limit) <> ("value" =: tshow v)
      elDynAttr "progress" attr blank
    cnt <- count newValEv
    (e,_) <- el' "p" $ do
      someVal <- holdDyn 0 newValEv
      display someVal
    pb <- getPostBuild
    let triggerEv = gate (current $ fmap (\c -> c < limit) cnt) (leftmost [pb, () <$ newValEv])
        doneEv = gate (current $ fmap (\c -> c == limit) cnt) (leftmost [() <$ newValEv])
    showTime "Start Time"
    widgetHold blank $ ffor doneEv $ \_ -> showTime "Done Time"

    -- Test
    newValEv <- performEvent $ ffor triggerEv $ \_ -> do
      (n :: Int32) <- liftIO $ do
        randomIO
      x <- DOM.getX =<< DOM.getBoundingClientRect (_element_raw e)
      pure $ n * (ceiling x)
    pure ()

websocketMsgThroughput :: (_) => m ()
websocketMsgThroughput = do
  el "h3" $ text "websocket msg throughput test"
  startEv <- do
    el "p" $ text "Enter number of msgs and delay to receive"
    v <- value <$> textInput (def & textInputConfig_initialValue .~ "1000")
    d <- value <$> textInput (def & textInputConfig_initialValue .~ "10000")
    text "milliseconds"
    ev <- button "Start"
    let vInt = tag (current $ liftA2 (,) <$> (readInt <$> v) <*> (readInt <$> d)) ev
    pure (fmapMaybe id vInt)

  uriVal <- value <$> textInput (def & textInputConfig_initialValue .~ "ws://localhost:8000/wstest")
  widgetHold_ blank $ ffor (attach (current uriVal) startEv) $ \(uri, (limit, delay)) -> mdo
    el "p" $ do
      let attr = ffor cnt $ \v -> ("max" =: tshow limit) <> ("value" =: tshow v)
      elDynAttr "progress" attr blank
    cnt <- count newValEv
    (e,_) <- el' "p" $ do
      someVal <- holdDyn 0 newValEv
      display someVal
    let
      doneEv = gate (current $ fmap (\c -> c == limit) cnt) (leftmost [() <$ newValEv])
    showTime "Start Time"
    widgetHold blank $ ffor doneEv $ \_ -> showTime "Done Time"

    -- Test
    let newValEv = fmapMaybe (\t -> readMay (T.unpack t)) recvEv
    (RawWebSocket recvEv openEv _ _) <- textWebSocket uri $ WebSocketConfig ([tshow (limit, delay)] <$ openEv) never False []
    pure ()
