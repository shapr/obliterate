{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Fix (MonadFix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Reflex
import Reflex.Dom
import Text.Read (readMaybe)

import Control.Monad
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route

import Reflex.Dom.Core

import Common.Api
import Common.Route

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend =
    Frontend
        { _frontend_head = do
            tutorial2
            -- tutorial3
            el "title" $ text "Obelisk Minimal Example"
            elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
        , _frontend_body = do
            el "h1" $ text "Shae, Welcome to Obelisk!"
            el "p" $ text $ T.pack commonStuff

            -- `prerender` and `prerender_` let you choose a widget to run on the server
            -- during prerendering and a different widget to run on the client with
            -- JavaScript. The following will generate a `blank` widget on the server and
            -- print "Hello, World!" on the client.
            prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

            elAttr "img" ("src" =: $(static "obelisk.jpg")) blank
            el "div" $ do
                exampleConfig <- getConfig "common/example"
                case exampleConfig of
                    Nothing -> text "No config file found in config/common/example"
                    Just s -> text $ T.decodeUtf8 s
            return ()
        }

tutorial2 :: DomBuilder t m => m ()
tutorial2 = el "div" $ do
              el "p" $ text "Reflex is: "
              el "ul" $ do
                el "li" $ text "Efficient"
                el "li" $ text "Higher Order"
                el "li" $ text "Glitch free"

tutorial3 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial3 = el "div" $ do
              t <- inputElement def
              text " "
              dynText $ _inputElement_value t
