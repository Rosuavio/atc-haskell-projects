{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor (void, ($>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Graphics.Vty (defaultConfig)
import qualified Graphics.Vty as VTY
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
    quitEv <- key $ VTY.KChar 'q'
    downEv <- key $ VTY.KChar 'j'
    upEv <- key $ VTY.KChar 'k'
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
              True -> fileView filePath $ leftmost [ downEv $> (1), upEv $> (-1) ]
      grout (fixed $ constDyn 1) $ text "q - quit | j/k - down/up"
    pure $ void quitEv

fileView ::
  forall t m.
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
  ) => OsPath -> Event t Int -> m ()
fileView filePath moveEv = do
  pb <- getPostBuild
  height <- displayHeight
  let
    topLine = 1
  rec
    let
      selectionMoved = attachWithMaybe
        (\curr delta ->
          let
          in Just 1
        )
        selectedLine
        moveEv
    selectedLine <- accum (+) topLine selectionMoved
  -- selectedLine <- foldDynMaybe (\curr delta ->
  --     let new = max topLine $ delta + curr
  --     in if new == curr then Nothing else Just new
  --   )
  --   topLine
  --   moveEv
  rec
    let
      needNLinesEv = attachWithMaybe
        calLinesToGet
        (current fileView)
        $ leftmost
          [ updated height
          , current height `tag` pb
          -- , updated selectedLine
          ]

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
  rec
    let
      -- TODO: I do not want to scroll with the selectedLine line
      -- this means it will try to keep the selected line on the top
      aaa = foo s 
      sConf = def {
        _scrollableConfig_scrollBy = aaa
      }
    (s, _) <- grout flex $ scrollable sConf $ col $ do
      hold Nothing (fmap Just aaa) >>= grout (fixed $ constDyn 1) . display
      grout (fixed $ constDyn 1) . display $ _scrollable_scrollPosition s
      grout (fixed $ constDyn 1) . display $ _scrollable_totalLines  s
      void $ networkView $ ffor (fvLines <$> fileView) $ \fileLines ->
        flip Seq.traverseWithIndex fileLines $ \i line ->
          let
            style = (def @(RichTextConfig t)) {
              _richTextConfig_attributes = ffor (current selectedLine) $ \sss ->
                if sss == i + 1
                  then VTY.currentAttr `VTY.withStyle` VTY.underline
                  else VTY.currentAttr
            }
          in
            grout (fixed $ constDyn 1) $ richText style $ constant line

      -- Event that signals an update to the contents of
      -- scrollable
      pure (never, ())
  pure ()
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

    calLinesToGet fv needLineNumb
      | Just fileBottom <- fvBottomOfFile fv
      , fileBottom <= fvBottomPos fv
      = Nothing
      | delta <= 0 = Nothing
      | otherwise = Just delta
      where
        delta = needLineNumb - Seq.length (fvLines fv)

data FileView
  = MkFileView
  { fvLines :: Seq.Seq T.Text
  , fvBottomPos :: Integer
  , fvBottomOfFile :: Maybe Integer
  }


{-
At: 30
Pos: 20
50 = 30 + 20
H: 50
C: +x
-}

foo :: Scrollable t -> Dynamic t Int -> Event t Int
foo scroll selected = flip push (updated selected) $ \newSelection -> do
  h <- sample $ _scrollable_scrollHeight scroll
  pos <- sample $ _scrollable_scrollPosition scroll
  numOfLines <- sample $ _scrollable_totalLines scroll
  currSelection <- sample $ current selected
  pure 5
  where
    y = case sPos of
      ScrollPos_Top -> currentPos
      ScrollPos_Bottom -> currentPos
      ScrollPos_Line l -> currentPos + l

-- foo ScrollPos_Top _ 1 = Nothing
-- foo _ _ 1 = Just ScrollPos_Top
-- foo ScrollPos_Bottom totalLines l | l > totalLines = Nothing
-- foo _ totalLines l | l > totalLines = Just ScrollPos_Bottom
-- foo _ _ l = Just $ ScrollPos_Line l
