{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

-- | A modal dialogue, eg pop up, for example to confirm an action.
--   Puts code behinds bulma's modal styling: https://bulma.io/documentation/components/modal/
module Reflex.Bulmex.Modal(modal, OnClose, modal', modalClose) where

import           Control.Lens
import           Control.Monad.Fix
import           Reflex
import           Reflex.Bulmex.Attr
import qualified Reflex.Dom.Builder.Class as Dom
import qualified Reflex.Dom.Widget.Basic  as Dom
import qualified Reflex.Tags              as T

data OnClose = OnClose

-- | A modal that opens on event and has a cross to close it.
modal :: (PostBuild t m, MonadHold t m, MonadFix m, Dom.DomBuilder t m) => Event t () -> m a -> m (a, Event t OnClose)
modal = modal' never

-- | A modal that can be opened and closed with events.
--   Also has a cross to close it.
modal' :: (PostBuild t m, MonadHold t m, MonadFix m, Dom.DomBuilder t m) => Event t () -> Event t () -> m a -> m (a, Event t OnClose)
modal' closeEvt openEvt monad =
  modalClose openEvt $ do
    res <- monad
    pure (res, closeEvt)

modalClose :: (PostBuild t m, MonadHold t m, MonadFix m, Dom.DomBuilder t m) => Event t () -> m (a, Event t ()) -> m (a, Event t OnClose)
modalClose openEvt monad = do
  rec stateDyn <- holdDyn closed (leftmost [opened <$ openEvt, closed <$ result ^. _2])
      result <- T.divDynAttr stateDyn $ do
          (backgroundEl, _) <- T.divAttr' (classAttr "modal-background") $ Dom.blank
          monadResult <- T.divClass "modal-content" $ monad
          (buttonEl, _) <- T.aAttr' (classAttr "modal-close is-large") $ Dom.blank
          let closeClick = leftmost [Dom.domEvent Dom.Click backgroundEl, Dom.domEvent Dom.Click buttonEl, monadResult ^. _2]
          pure $ (monadResult ^. _1, difference closeClick openEvt)
  pure $ over _2 ((<$) OnClose) result
  where
    closed = classAttr "modal"
    opened = classAttr "modal is-active"
