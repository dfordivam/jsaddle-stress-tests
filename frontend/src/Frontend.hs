{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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

import qualified Tests

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "GHCJS / JSaddle interface Tests"
  , _frontend_body = do
      prerender (pure ()) $ do
        Tests.main
      return ()
  }
