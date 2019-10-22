{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Form capture enter presses and provide an html like control flow.
--   They also indicate to the child monad how an action is performing
--   with spinstates.
module Reflex.Bulmex.Form
  ( actionForm
  , form
  -- * Spin
  , withSpinDyn
  , spinWidget
  , aSpinButtonClass
  , loadAttr
  -- * Types
  , module X
  ) where

import           Control.Lens
import           Control.Monad.Fix
import           Control.Monad.IO.Class       (MonadIO)
import qualified Data.Map.Strict              as Map
import qualified Data.Text                    as Text
import           Reflex
import           Reflex.Bulmex.Attr
import           Reflex.Bulmex.Event
import           Reflex.Bulmex.Form.FormTypes as X
import           Reflex.Bulmex.Input.Basic
import qualified Reflex.Dom.Builder.Class     as Dom
import qualified Reflex.Dom.Widget            as Dom
import qualified Reflex.Tags                  as T
import qualified Web.KeyCode                  as Dom

-- | The first argument is a function that expects data `a`,
--   and a trigger event, producing another event in a monadic context.
--   This perfectly aligns with servant-reflex.
--   The produced event is given to the form, the second argument.
--   which also includes the 'SpinState', indicating if we're
--   exeecuting the event or not.
--   The form has to return 3 values, firstly `a` the information for the first function,
--   The is the event that controls the form, and the third is an optional return value.
actionForm ::
     (Dom.DomBuilder t m, MonadHold t m, MonadFix m)
  => (a -> Event t () -> m (Event t b))
  -> (Event t b -> Dynamic t SpinState -> m (a, Event t FormAction, c))
  -> m c
actionForm actF monM =
  form $ \onEnter -> do
    rec let action = formDat ^. _2
            sendEvt =
              onEnter <> (() <$ noNothing (preview _PostDefault <$> action))
            startSpin =
              sendEvt <> (() <$ noNothing (preview _Loading <$> action))
            stopSpin =
              (() <$ reqRes) <> (() <$ noNothing (preview _FormRest <$> action))
        state <- spinState startSpin stopSpin
        formDat <- monM reqRes state
        reqRes <- actF (formDat ^. _1) sendEvt
    pure $ formDat ^. _3

spinState ::
     (Reflex t, MonadHold t m, MonadFix m)
  => Event t ()
  -> Event t ()
  -> m (Dynamic t SpinState)
spinState start stop =
  accumDyn (const id) SpinRest $ leftmost [Spinning <$ start, SpinRest <$ stop]

-- This looks a lot like `withDebounceEvtReq`, maybe a better abstraction is possible?
-- | A more general use of the spinstate.
--   The first argument is the widget that can indicate when to execute
--   the second function. It will be made aware of the 'SpinState'.
spinWidget ::
     ( Dom.DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , (MonadIO (Performable m))
     , MonadHold t m
     , MonadFix m
     )
  => (Dynamic t SpinState -> m (Event t ()))
  -> (Event t () -> m (Event t b))
  -> m (Event t b)
spinWidget widgetF eventHandlr = do
  rec onClick <- widgetF dynamicClass
      onRequest <- eventHandlr onClick
      let setClassOnReq = () <$ onRequest
      setClassafterReq <- delay 0 setClassOnReq -- on redraws it needs a delay
      dynamicClass <- spinState onClick $ setClassOnReq <> setClassafterReq
  pure onRequest

loadAttr :: SpinState -> AttrMap
loadAttr SpinRest = mempty
loadAttr Spinning = Map.fromList [("class", "is-loading"), ("disabled", "1")]

withSpinDyn -- XXX only used once? maybe remove
 ::
     ( Dom.DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , (MonadIO (Performable m))
     , MonadHold t m
     , MonadFix m
     )
  => AttrMap
  -> (Dynamic t AttrMap -> m (Event t ()))
  -> (Event t () -> m (Event t b))
  -> m (Event t b)
withSpinDyn atrributes f =
  spinWidget (f . fmap (attrUnion atrributes . loadAttr))

aSpinButtonClass ::
     (Dom.DomBuilder t m, PostBuild t m)
  => Text.Text
  -> Dynamic t SpinState
  -> m ()
  -> m (Event t ())
aSpinButtonClass clazz spinstate =
  fmap fst .
  abuttonDynAttr (attrUnion (classAttr clazz) . loadAttr <$> spinstate)

-- attrmap
-- | A form captures enter presses of child componetns and
--   sends it to them in an event.
form :: (Dom.DomBuilder t m, MonadFix m) => (Event t () -> m a) -> m a
form monF = do
  rec val <-
        T.formAttr'
          -- sometimes it reloads on enter, not always, this stops that
          (Map.singleton "onsubmit" "return false;") $
        monF enter
      let enter = Dom.keypress Dom.Enter (val ^. _1)
  pure $ val ^. _2
