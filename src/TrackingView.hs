module TrackingView
  ( trackingView
  ) where

import Control.Monad.Fix (MonadFix)
import Graphics.Vty.Image (cropLeft, cropTop, imageHeight, imageWidth)

import Reflex
import Reflex.Vty

trackingView ::
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  , HasImageWriter t m
  , HasDisplayRegion t m
  )
  => Dynamic t Region -> m a -> m a
trackingView target mkImg = askRegion
  >>= accumB movePos (0, 0) . updated . liftA2 (,) target
  >>= flip mapImages mkImg . liftA2 (fmap . crop')
  where
    movePos (left, top) (t, r) =
      -- NOTE: We reliy on the order of min and max here. This happens to be the
      -- same order as clamp, but using clamp would be to obscure.
      ( min targetLeft $ max (targetRight - _region_width r) left
      , min targetTop $ max (targetBottom - _region_height r) top
      )
      where
        targetLeft = _region_left t
        targetTop = _region_top t
        targetRight = targetLeft + _region_width t
        targetBottom = targetTop + _region_height t

    crop' (left, top) img =
      cropLeft (max 0 $ imageWidth img - left)
      $ cropTop (max 0 $ imageHeight img - top)
      img
