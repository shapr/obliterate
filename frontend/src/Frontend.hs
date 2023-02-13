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

            el "title" $ text "Obelisk Minimal Example"
            elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
        , _frontend_body = do
            tutorial11
            -- el "h1" $ text "Shae, Welcome to Obelisk!"
            -- el "p" $ text $ T.pack commonStuff

            -- -- `prerender` and `prerender_` let you choose a widget to run on the server
            -- -- during prerendering and a different widget to run on the client with
            -- -- JavaScript. The following will generate a `blank` widget on the server and
            -- -- print "Hello, World!" on the client.
            -- prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

            -- elAttr "img" ("src" =: $(static "obelisk.jpg")) blank
            -- el "div" $ do
            --     exampleConfig <- getConfig "common/example"
            --     case exampleConfig of
            --         Nothing -> text "No config file found in config/common/example"
            --         Just s -> text $ T.decodeUtf8 s
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

tutorial4 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial4 = el "div" $ do
              t <- inputElement $ def
                   & inputElementConfig_initialValue .~ "0"
                   & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number") -- aha, this creates a Map of html element attributes, but with only one pair?
              text " "
              dynText $ _inputElement_value t

tutorial5 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial5 = el "div" $ do
              x <- numberInput
              let numberString = fmap (pack . show) x
              text " "
              dynText numberString
                  where
                    numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
                    numberInput = do
                         n <- inputElement $ def
                              & inputElementConfig_initialValue .~ "0"
                              & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
                         pure . fmap (readMaybe . unpack) $ _inputElement_value n

tutorial6 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial6 = el "div" $ do
              nx <- numberInput
              text " + "
              ny <- numberInput
              text " = "
              let result = zipDynWith (\x y -> (+) <$> x <*> y) nx ny
                  resultString = fmap (pack . show) result
              dynText resultString
                  where
                    numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
                    numberInput = do
                         n <- inputElement $ def
                              & inputElementConfig_initialValue .~ "0"
                              & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
                         pure . fmap (readMaybe . unpack) $ _inputElement_value n

data Op = Plus | Minus | Times | Divide
        deriving (Eq, Ord, Show)

runOp :: Fractional a => Op -> a -> a -> a
runOp s = case s of
            Plus -> (+)
            Minus -> (-)
            Times -> (*)
            Divide -> (/)

ops :: Map Op Text
ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]

tutorial7 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
tutorial7 = el "div" $ do
              nx <- numberInput
              op <- _dropdown_value <$> dropdown Times (constDyn ops) def
              ny <- numberInput
              let values = zipDynWith (,) nx ny
                  result = zipDynWith (\o (x,y) -> runOp o <$> x <*> y) op values
                  resultText = fmap (pack . show) result
              text " = "
              dynText resultText
                  where
                    numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
                    numberInput = do
                         n <- inputElement $ def
                              & inputElementConfig_initialValue .~ "0"
                              & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
                         pure . fmap (readMaybe . unpack) $ _inputElement_value n

buttonClass :: DomBuilder t m => Text -> Text -> m (Event t ())
buttonClass c s = do
  (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: c) $ text s
  pure $ domEvent Click e

numberPad :: (DomBuilder t m) => m (Event t Text)
numberPad = do
  b7 <- ("7" <$) <$> numberButton "7"
  b8 <- ("8" <$) <$> numberButton "8"
  b9 <- ("9" <$) <$> numberButton "9"
  b4 <- ("4" <$) <$> numberButton "4"
  b5 <- ("5" <$) <$> numberButton "5"
  b6 <- ("6" <$) <$> numberButton "6"
  b1 <- ("1" <$) <$> numberButton "1"
  b2 <- ("2" <$) <$> numberButton "2"
  b3 <- ("3" <$) <$> numberButton "3"
  b0 <- ("0" <$) <$> numberButton "0"
  pure $ leftmost [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]
      where
        numberButton n = buttonClass "number" n

tutorial8 :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial8 = el "div" $ do
              numberButton <- numberPad
              clearButton <- button "C"
              let buttons = leftmost
                            [ Nothing <$ clearButton
                            , Just <$> numberButton
                            ]
              dstate <- accumDyn collectButtonPresses initialState buttons
              text " "
              dynText dstate
                  where
                    initialState :: Text
                    initialState = T.empty

                    collectButtonPresses :: Text -> Maybe Text -> Text
                    collectButtonPresses state buttonPress =
                        case buttonPress of
                          Nothing -> initialState
                          Just digit -> state <> digit

-- minimal four function calculator

data CalcState = CalcState
               { _calcState_acc :: Double -- accumulator
               , _calcState_op :: Maybe Op -- most recently requested operation
               , _calcState_input :: Text -- current input
               } deriving (Show)

data Button
    = ButtonNumber Text
    | ButtonOp Op
    | ButtonEq
    | ButtonClear

initCalcState :: CalcState
initCalcState = CalcState 0 Nothing ""

updateCalcState :: CalcState -> Button -> CalcState
updateCalcState state@(CalcState acc mOp input) btn =
    case btn of
      ButtonNumber d ->
          if d == "." && T.find (== '.') input /= Nothing
          then state
          else CalcState acc mOp (input <> d)
      ButtonOp pusherOp -> applyOp state (Just pusherOp)
      ButtonEq -> applyOp state Nothing
      ButtonClear -> initCalcState

applyOp :: CalcState -> Maybe Op -> CalcState
applyOp state@(CalcState acc mOp input) mOp' =
    if T.null input
    then CalcState acc mOp' input
    else
        case readMaybe (unpack input) of
          Nothing -> state -- should be unreachable
          Just x -> case mOp of
                      Nothing -> CalcState x mOp' ""
                      Just op -> CalcState (runOp op acc x) mOp' ""

displayCalcState :: CalcState -> Text
displayCalcState (CalcState acc _op input) =
    if T.null input
    then T.pack (show acc)
    else input


tutorial9 :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial9 = el "div" $ do
              numberButtons <- numberPad
              bPeriod <- ("." <$) <$> button "."
              bPlus <- (Plus <$) <$> button "+"
              bMinus <- (Minus <$) <$> button "-"
              bTimes <- (Times <$) <$> button "*"
              bDivide <- (Divide <$) <$> button "/"
              let opButtons = leftmost [bPlus, bMinus, bTimes, bDivide]
              bEq <- button "="
              bClear <- button "C"
              let buttons = leftmost
                            [ ButtonNumber <$> numberButtons
                            , ButtonNumber <$> bPeriod
                            , ButtonOp <$> opButtons
                            , ButtonEq <$ bEq
                            , ButtonClear <$ bClear
                            ]
              calcState <- accumDyn updateCalcState initCalcState buttons
              text " "
              dynText (displayCalcState <$> calcState)

tutorial10 :: forall t m. (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial10 = el "div" $ do
               rec
                 numberButtons <- numberPad
                 bPeriod <- ("." <$) <$> button "."
                 let opState = _calcState_op <$> calcState
                 bPlus <- opButton Plus "+" opState
                 bMinus <- opButton Minus "-" opState
                 bTimes <- opButton Times "*" opState
                 bDivide <- opButton Divide "/" opState
                 let opButtons = leftmost [bPlus, bMinus, bTimes, bDivide]
                 bEq <- button "="
                 bClear <- button "C"
                 let buttons = leftmost
                               [ ButtonNumber <$> numberButtons
                               , ButtonNumber <$> bPeriod
                               , ButtonOp <$> opButtons
                               , ButtonEq <$ bEq
                               , ButtonClear <$ bClear
                               ]
                 calcState <- accumDyn updateCalcState initCalcState buttons
                 text " "
                 dynText (T.pack . show . _calcState_acc <$> calcState)
                 el "br" blank
                 dynText (_calcState_input <$> calcState)
               pure ()
                   where
                     opButton :: Op -> Text -> Dynamic t (Maybe Op) -> m (Event t Op)
                     opButton op label selectedOp = do
                          (e, _) <- elDynAttr' "button" (pickColor <$> selectedOp) $ text label
                          pure (op <$ domEvent Click e)
                              where
                                pickColor mOp =
                                    if Just op == mOp
                                    then "style" =: "color: red"
                                    else Map.empty

tutorial11 ::forall t m. (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial11 = divClass "calculator" $ do
               rec
                 divClass "output" $ dynText $ displayCalcState <$> calcState
                 buttons <- divClass "input" $ do
                                       (numberButtons, bPeriod) <- divClass "number-pad" $ do
                                                                     numberButtons <- numberPad
                                                                     bPeriod <- ("." <$) <$> buttonClass "number" "."
                                                                     pure (numberButtons, bPeriod)
                                       (opButtons, bEq) <- divClass "ops-pad" $ do
                                                             let opState = _calcState_op <$> calcState
                                                             bPlus <- opButton Plus "+" opState
                                                             bMinus <- opButton Minus "-" opState
                                                             bTimes <- opButton Times "*" opState
                                                             bDivide <- opButton Divide "/" opState
                                                             let opButtons = leftmost [bPlus, bMinus, bTimes, bDivide]
                                                             bEq <- buttonClass "primary" "="
                                                             pure (opButtons, bEq)
                                       bClear <- divClass "other-pad" $ do
                                                   bClear <- buttonClass "secondary" "C"
                                                   _ <- buttonClass "secondary" "+/-"
                                                   _ <- buttonClass "secondary" "%"
                                                   pure bClear
                                       let buttons = leftmost
                                                     [ ButtonNumber <$> numberButtons
                                                     , ButtonNumber <$> bPeriod
                                                     , ButtonOp <$> opButtons
                                                     , ButtonEq <$ bEq
                                                     , ButtonClear <$ bClear
                                                     ]
                                       pure buttons
                 calcState <- accumDyn updateCalcState initCalcState buttons
               pure ()
                   where
                     opButton :: Op -> Text -> Dynamic t (Maybe Op) -> m (Event t Op)
                     opButton op label selectedOp = do
                                       (e, _) <- elDynAttr' "button" (("class" =: "primary" <>) <$> (pickColor <$> selectedOp)) $ text label
                                       pure (op <$ domEvent Click e)
                                           where
                                             pickColor mOp =
                                                 if Just op == mOp
                                                 then "style" =: "background: lightblue"
                                                 else Map.empty
