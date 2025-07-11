{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (for_)
import Data.Functor (void, ($>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Graphics.Vty (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)
import Reflex
import Reflex.Network
import Reflex.Vty
import System.Directory.OsPath
  ( Permissions (readable)
  , doesFileExist
  , getPermissions
  )
import System.File.OsPath (withFile)
import System.IO
  ( IOMode (ReadMode)
  , SeekMode (AbsoluteSeek)
  , hIsEOF
  , hSeek
  , hTell
  )

import Control.Monad.Fix (MonadFix)
import System.OsPath (OsPath)
import Util

main :: IO ()
main = do
  vty <- mkVty defaultConfig
  mainWidgetWithHandle vty $ initManager_ $ do
    quitEv <- input
    pb <- getPostBuild
    defaultFileInfoEv <- performEventAsync $
      pb $> \onComplete -> liftIO $ void $ forkIO $ do
        f <- getDefaultFile
        canRead <- doesFileExist f >>= \case
          False -> pure False
          True -> readable <$> getPermissions f
        onComplete (f, canRead)
    grout flex $ col $ do
      void $ grout flex $ do
        networkHold
          (text "Loading default file...")
          $ ffor defaultFileInfoEv
            $ \(filePath, canRead) -> col $ case canRead of
              False -> text $ constant $ "Can't raad file: "
                <> T.pack (show filePath)
              True -> fileView filePath
      grout (fixed $ constDyn 1) $ text "Press any key to continue..."
    pure $ void quitEv

fileView ::
  ( PostBuild t m
  , TriggerEvent t m
  , PerformEvent t m
  , MonadIO (Performable m)
  , Adjustable t m
  , HasInput t m
  , HasLayout t m
  , HasDisplayRegion t m
  , HasImageWriter t m
  , HasTheme t m
  , HasFocusReader t m
  , MonadHold t m
  , MonadFix m
  , NotReady t m
  ) => OsPath -> m ()
fileView filePath = do
  pb <- getPostBuild
  height <- displayHeight
  rec
    let
      needNLinesEv = attachWithMaybe
        calLinesToGet
        (current fileView)
        $ leftmost [ updated height, current height `tag` pb]

    -- TODO: This can be running while a new needNLinesEv comes through...
    -- Figure the more effecent way of dealing with it.
    -- Right now, it seems like the IO happens to complete in the same sequence
    -- that the needNLinesEv come in. This is good because we dont get lines
    -- back out of order, but it only happens to work (maybe because `withFile`
    -- locks the reasource and holds up next `withFile`s untill it closes.
    --
    -- What we can do?
    -- If new needNLinesEv comes before this returns we can cancel it or
    -- something and let the new IO happen. It should still old bottomPos and
    -- a the lines requested should be more.
    --
    -- Maybe use some kind of debounceing
    gotFileView <- performEventAsync
      $ ffor (attach (current fileView) needNLinesEv)
        $ \(fv, getNMoreLines) onComplete -> liftIO $ void $
          forkIO $ getLinesFromPos (fvBottomPos fv) getNMoreLines
            >>= onComplete

    fileView <- accum
      (\curr new -> new { fvLines = fvLines curr <> fvLines new })
      MkFileView
        { fvLines = mempty
        , fvBottomPos = 0
        , fvBottomOfFile = Nothing
        }
      gotFileView
  fmap snd $ grout flex $ scrollable def $ col $ do
    void $ networkView $ ffor (fvLines <$> fileView) $ \fileLines ->
      for_ fileLines $ \line ->
        grout (fixed $ constDyn 1) $ text $ constant line

    -- Event that signals an update to the contents of
    -- scrollable
    pure (never, ())
  where
    getLinesFromPos pos numToGet = withFile filePath ReadMode $ \fh -> do
      hSeek fh AbsoluteSeek pos
      newLines <- hGetNLines fh numToGet
      nB <- hTell fh
      eof <- hIsEOF fh
      pure MkFileView
        { fvLines = newLines
        , fvBottomPos = nB
        , fvBottomOfFile = if eof then Just nB else Nothing
        }

    calLinesToGet fv heightOfScreen
      | Just fileBottom <- fvBottomOfFile fv
      , fileBottom <= fvBottomPos fv
      = Nothing
      | delta <= 0 = Nothing
      | otherwise = Just delta
      where
        delta = heightOfScreen - Seq.length (fvLines fv)

data FileView
  = MkFileView
  { fvLines :: Seq.Seq T.Text
  , fvBottomPos :: Integer
  , fvBottomOfFile :: Maybe Integer
  }
