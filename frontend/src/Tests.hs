{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests (main) where

import Safe
import Control.Applicative
import Control.Monad.IO.Class
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time

import qualified GHCJS.DOM.DOMRectReadOnly as DOM
import qualified GHCJS.DOM.Element as DOM
import Reflex.Dom.Core
import System.Random

main :: (_) => m ()
main = do
  throughputTest
  el "hr" blank
  websocketMsgThroughput

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

readInt :: T.Text -> Maybe Int
readInt = readMay . T.unpack

showTime lbl = el "p" $ do
  t <- liftIO getCurrentTime
  text (lbl <> " : " <> tshow t)
  pure t

throughputTest :: (_) => m ()
throughputTest = do
  el "h3" $ text "Throughput Test: DOM API Calls"
  startEv <- do
    el "p" $ text "Enter number of requests to do"
    v <- value <$> textInput (def & textInputConfig_initialValue .~ "10000")
    ev <- button "Start"
    let vInt = tag (current $ readInt <$> v) ev
    pure (fmapMaybe id vInt)

  widgetHold_ blank $ ffor startEv $ \limit -> mdo
    el "p" $ do
      let attr = ffor cnt $ \v -> ("max" =: tshow limit) <> ("value" =: tshow v)
      elDynAttr "progress" attr blank
      el "div" $ do
        text "("
        dynText $ tshow <$> cnt
        text ")"
    cnt <- count newValEv
    (e,_) <- el' "p" $ do
      someVal <- holdDyn 0 newValEv
      text "Some random number :"
      display someVal
    pb <- getPostBuild
    let triggerEv = gate (current $ fmap (\c -> c < limit) cnt) (leftmost [pb, () <$ newValEv])
        doneEv = gate (current $ fmap (\c -> c == limit) cnt) (leftmost [() <$ newValEv])
    startTime <- showTime "Start Time"
    widgetHold blank $ ffor doneEv $ \_ -> do
      doneTime <- showTime "Done Time"
      text $ "Time Elapsed : " <> (tshow $ diffUTCTime doneTime startTime)

    -- Test
    newValEv <- performEvent $ ffor triggerEv $ \_ -> do
      (n :: Int32) <- liftIO $ do
        randomIO
      x <- DOM.getX =<< DOM.getBoundingClientRect (_element_raw e)
      pure $ n * (ceiling x)
    pure ()

websocketMsgThroughput :: (_) => m ()
websocketMsgThroughput = do
  el "h3" $ text "Stress Test: Incoming Websocket Messages"

  el "p" $ text "WebSocket URL"
  uriVal <- value <$> textInput (def & textInputConfig_initialValue .~ "ws://localhost:8000/wstest")

  startEv <- do
    el "p" $ text "Enter the number of messages to receive"
    v <- value <$> textInput (def & textInputConfig_initialValue .~ "1000")
    el "p" $ text "Delay between each message (in microseconds)"
    d <- value <$> textInput (def & textInputConfig_initialValue .~ "10000")
    ev <- button "Start"
    let vInt = tag (current $ liftA2 (,) <$> (readInt <$> v) <*> (readInt <$> d)) ev
    pure (fmapMaybe id vInt)

  widgetHold_ blank $ ffor (attach (current uriVal) startEv) $ \(uri, (limit, delay)) -> mdo
    el "p" $ do
      let attr = ffor cnt $ \v -> ("max" =: tshow limit) <> ("value" =: tshow v)
      elDynAttr "progress" attr blank
      el "div" $ do
        text "("
        dynText $ tshow <$> cnt
        text ")"
    cnt <- count newValEv
    (e,_) <- el' "p" $ do
      someVal <- holdDyn 0 newValEv
      display someVal
    let
      doneEv = gate (current $ fmap (\c -> c == limit) cnt) (leftmost [() <$ newValEv])
    startTime <- showTime "Start Time"
    widgetHold blank $ ffor doneEv $ \_ -> do
      doneTime <- showTime "Done Time"
      text $ "Time Elapsed : " <> (tshow $ diffUTCTime doneTime startTime)

    -- Test
    let newValEv = fmapMaybe (\t -> readMay (T.unpack t)) recvEv
    (RawWebSocket recvEv openEv _ _) <- textWebSocket uri $ WebSocketConfig ([tshow (limit, delay)] <$ openEv) never False []
    pure ()
