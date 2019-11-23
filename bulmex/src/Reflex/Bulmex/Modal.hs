{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

-- | A modal dialogue, for example to confirm an action.
--   Makes bulma's <https://bulma.io/documentation/components/modal/ modal> styling work
module Reflex.Bulmex.Modal
  ( modal
  , modal'
  , modalClose
  , OnClose
  ) where

import           Control.Lens
import           Control.Monad.Fix
import           Reflex
import           Reflex.Bulmex.Attr
import qualified Reflex.Dom.Builder.Class as Dom
import qualified Reflex.Dom.Widget.Basic  as Dom
import qualified Reflex.Tags              as T


-- | A modal that opens on event and has a cross to close it.
--   m a dictates what's inside the modal.
--   You probably want to use a 'Reflex.Bulmex.Tag.Bulma.box'.
modal ::
     (PostBuild t m, MonadHold t m, MonadFix m, Dom.DomBuilder t m)
  => Event t () -- ^ open trigger
  -> m a -- ^ modal body
  -> m (a, Event t OnClose)
modal = modal' never

-- | A modal that can be opened and closed with events.
--   It also has a cross to close with.
modal' ::
     (PostBuild t m, MonadHold t m, MonadFix m, Dom.DomBuilder t m)
  => Event t () -- ^ close trigger
  -> Event t () -- ^ open trigger
  -> m a -- ^ modal body
  -> m (a, Event t OnClose)
modal' closeEvt openEvt monad =
  modalClose openEvt $ do
    res <- monad
    pure (res, closeEvt)

-- | The most generic modal, It receives an open event.
--   And the inner monad can indicate when to close with a closeEvent.
modalClose ::
     (PostBuild t m, MonadHold t m, MonadFix m, Dom.DomBuilder t m)
  => Event t () -- ^ open trigger
  -> m (a, Event t ()) -- ^ body + close trigger result
  -> m (a, Event t OnClose)
modalClose openEvt monad = do
  rec stateDyn <-
        holdDyn closed (leftmost [opened <$ openEvt, closed <$ result ^. _2])
      result <-
        T.divDynAttr stateDyn $ do
          (backgroundEl, _) <-
            T.divAttr' (classAttr "modal-background") $ Dom.blank
          monadResult <- T.divClass "modal-content" $ monad
          (buttonEl, _) <-
            T.aAttr' (classAttr "modal-close is-large") $ Dom.blank
          let closeClick =
                leftmost
                  [ Dom.domEvent Dom.Click backgroundEl
                  , Dom.domEvent Dom.Click buttonEl
                  , monadResult ^. _2
                  ]
          pure $ (monadResult ^. _1, difference closeClick openEvt)
  pure $ over _2 ((<$) OnClose) result
  where
    closed = classAttr "modal"
    opened = classAttr "modal is-active"

-- | An incation that an event came from closing a modal
data OnClose =
  OnClose
