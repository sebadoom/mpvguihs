{-# LANGUAGE TemplateHaskell #-}

module Main where

import MpvPlayer.Frontend.GUITemplates
import MpvPlayer.Backend

import Data.IORef
import Data.Maybe
import Data.Char
import System.Directory
import System.FilePath
import qualified System.IO as SIO
import Control.Monad
import Control.Monad.Reader
import System.Environment
import System.Directory

data App = App {
      appHandles :: Handles,
      appPlayer  :: Maybe (IORef Player), 
      appToggleSigId :: Maybe (ConnectId ToggleToolButton),
      appVolSigId :: Maybe (ConnectId VolumeButton),
      appStatusContextId :: ContextId,
      appCmdLine :: String
}

confFile :: IO FilePath
confFile = do
  dir <- getAppUserDataDirectory "mpvguihs"
  exists <- doesDirectoryExist dir
  when (not exists) $ createDirectoryIfMissing True dir
  return $ dir `combine` "mpvguihs.conf"

openFile :: IORef App -> FilePath -> IO ()
openFile appRef filename = do
  app <- readIORef appRef
  when (isJust $ appPlayer app) $ do
    let p = fromJust $ appPlayer app
    mpvStop p
    mpvTerminate p

  let fsWin = fullscreenWindow $ appHandles app
  let box = background $ appHandles app
  let bgImage = backgroundImage $ appHandles app
  containerRemove box bgImage
  socket <- socketNew
  set socket [widgetCanFocus := True, 
              widgetSensitive := True]
  widgetAddEvents socket [AllEventsMask]
  containerAdd box socket
  widgetShow socket
  on box enterNotifyEvent $ tryEvent $ liftIO $ widgetGrabFocus socket
  on fsWin enterNotifyEvent $ tryEvent $ liftIO $ widgetGrabFocus socket
  wid <- socketGetId socket

  playerRef <- mpvPlay (fromNativeWindowId wid) filename (appCmdLine app)

  writeIORef appRef (app { appPlayer = Just playerRef })

showOpenDialog :: IORef App -> IO ()
showOpenDialog appRef = do
  app <- readIORef appRef
  dialog <- fileChooserDialogNew Nothing 
            (Just $ mainWindow $ appHandles app) FileChooserActionOpen []
  dialogAddButton dialog "gtk-cancel" $ ResponseUser 0
  dialogAddButton dialog "gtk-open" $ ResponseUser 1
  ResponseUser resp <- dialogRun dialog
  
  when (resp == 1) $ do file <- fileChooserGetFilename dialog
                        case file of
                          Just fn -> openFile appRef fn
                          Nothing -> return ()
 
  widgetDestroy dialog

setCmdLine appRef cmdLine = do
  modifyIORef appRef $ \app -> app { appCmdLine = cmdLine }
  cf <- confFile
  h <- SIO.openFile cf SIO.WriteMode
  SIO.hPutStr h cmdLine
  SIO.hClose h

showSettingsDialog :: IORef App -> IO ()
showSettingsDialog appRef = do
  app <- readIORef appRef
  let hs = appHandles app
  let s = settingsDialog $ appHandles app
  entrySetText (cmdLineEntry hs) (appCmdLine app)
  r <- dialogRun s
  print r
  case r of
    ResponseUser 1 -> do 
      cmdLine <- entryGetText (cmdLineEntry hs)
      setCmdLine appRef cmdLine

    _              -> return ()

  widgetHide s

showAboutDialog :: IORef App -> IO ()
showAboutDialog appRef = do
  app <- readIORef appRef
  let d = aboutDialog $ appHandles app
  void $ dialogRun d
  widgetHide d

playToggle :: IORef App -> IO ()
playToggle appRef = do
  putStrLn "lol"
  app <- readIORef appRef
  pressed <- toggleToolButtonGetActive (playButton $ appHandles app)
  
  case (appPlayer app) of
    Just p -> if pressed 
              then mpvUnpause p
              else mpvPause p
    Nothing -> return ()

seek :: IORef App -> Double -> IO ()
seek appRef value = do
  app <- readIORef appRef
  when (isJust $ appPlayer app) $ 
    mpvSeek (fromJust $ appPlayer app) value

setVolume :: IORef App -> Double -> IO ()
setVolume appRef vol = do
  app <- readIORef appRef
  when (isJust $ appPlayer app) $
    mpvSetVolume (fromJust $ appPlayer app) $ round (vol * 100)

toggleFullscreen :: IORef App -> IO ()
toggleFullscreen appRef = do
  app <- readIORef appRef
  when (isJust $ appPlayer app) $
    mpvToggleFullscreen (fromJust $ appPlayer app)

connectSignals :: IORef App -> IO ()
connectSignals appRef = do
  app <- readIORef appRef
  let hs = appHandles app

  onDestroy (mainWindow hs) mainQuit
  onDestroy (fullscreenWindow hs) mainQuit

  onToolButtonClicked (openButton hs) $ showOpenDialog appRef
  idPlay <- afterToolButtonToggled (playButton hs) $ playToggle appRef
                      
  onAdjustBounds (scale hs) $ seek appRef

  idVol <- on (volumeButton hs) scaleButtonValueChanged $ setVolume appRef

  onToolButtonClicked (fullscreenButton hs) $ toggleFullscreen appRef

  onToolButtonClicked (aboutButton hs) $ showAboutDialog appRef
  onToolButtonClicked (settingsButton hs) $ showSettingsDialog appRef
  
  writeIORef appRef app { appToggleSigId = Just idPlay,
                          appVolSigId = Just idVol }

prepareUI :: Handles -> IO Handles
prepareUI hs = do
  windowSetTitle (mainWindow hs) "mpv GUI"

  -- HACK: volume button shows no icon when loaded from Glade.
  p <- widgetGetParent (volumeButton hs)
  let box = castToBox $ fromJust p
  widgetDestroy (volumeButton hs)
  volBut <- volumeButtonNew
  boxPackEnd box volBut PackNatural 0

  let s = settingsDialog hs
  dialogAddButton s "gtk-cancel" $ ResponseUser 0
  dialogAddButton s "gtk-ok" $ ResponseUser 1

  widgetModifyBg (background hs) StateNormal $ Color 0 0 0

  return $ hs { volumeButton = volBut }

formatPlayMessage :: Double -> Double -> String
formatPlayMessage pos total = concat 
    [str $ hours pos  ,":", str $ mins pos  ,":",str $ secs pos, " / ",
     str $ hours total,":", str $ mins total,":",str $ secs total]
    where comp t = round $ 60 * (snd $ properFraction t)
          str t = if t < 10 then "0" ++ show t else show t
          secs t = comp $ t / 60 :: Int
          mins t = comp $ t / 60 / 60 :: Int
          hours t = round $ t / 60 / 60 / 60 :: Int

checkFullscreen :: IORef App -> PlayStatus -> IO ()
checkFullscreen appRef status = do
  app <- readIORef appRef
  let fsWin = fullscreenWindow $ appHandles app
  let normalWin = mainWindow $ appHandles app
  let box = background $ appHandles app

  let fsFlag = playStatusFullscreen status
  fs <- widgetGetMapped fsWin

  when (fsFlag && (not fs)) $ do
    cs <- containerGetChildren box
    let socket = head cs
    widgetShowAll fsWin
    widgetReparent socket fsWin
    windowFullscreen fsWin
    widgetHide normalWin

  when ((not fsFlag) && fs) $ do
    cs <- containerGetChildren fsWin
    let socket = head cs
    widgetReparent socket box
    widgetShowAll normalWin
    windowUnfullscreen fsWin
    widgetHide fsWin

updateUI :: IORef App -> IO Bool
updateUI appRef = do
  app <- readIORef appRef
  let hs = appHandles app

  signalBlock $ fromJust $ appToggleSigId app
  signalBlock $ fromJust $ appVolSigId app

  let playing = isJust $ appPlayer app

  widgetSetSensitive (scale hs) playing
  widgetSetSensitive (playButton hs) playing
  widgetSetSensitive (fullscreenButton hs) playing

  if playing
     then do
       let p = fromJust $ appPlayer app
       status <- mpvGetPlayStatus p
       when (isJust status) $ do
         let s = fromJust status
         let ratio = (playStatusPos s) / (playStatusLength s)
         rangeSetValue (scale hs) ratio
         set (playButton hs) [toggleToolButtonActive := 
                              (not $ playStatusPaused s)]
         set (volumeButton hs) [scaleButtonValue := 
                                (fromIntegral $ playStatusVol s) / 100]
         statusbarPop (statusbar hs) (appStatusContextId app)
         void $ statusbarPush (statusbar hs) (appStatusContextId app) $ 
           formatPlayMessage (playStatusPos s) (playStatusLength s)
         
         checkFullscreen appRef s
         
     else do 
       rangeSetValue (scale hs) 0.0
       set (playButton hs) [toggleToolButtonActive := False]
       statusbarRemoveAll (statusbar hs) (fromIntegral $ appStatusContextId app)

  signalUnblock $ fromJust $ appToggleSigId app
  signalUnblock $ fromJust $ appVolSigId app
                 
  return True

loadCmdLine :: IO String
loadCmdLine = do
  cf <- confFile 
  exists <- doesFileExist cf
  if exists 
     then do str <- readFile cf
             return $ case filter (any isAlpha) $ lines str of
               (l:_) -> l
               _     -> ""
     else return ""

main :: IO ()
main = do
  initGUI
  builder <- builderNew
  builderAddFromFile builder "main-gtk2.ui"
  hs <- prepareUI =<< $(getHandlesExp [| builder |])

  statusId <- statusbarGetContextId (statusbar hs) "SimpleStatus" 

  cmdLine <- loadCmdLine
  
  appRef <- newIORef $ App hs Nothing Nothing Nothing statusId cmdLine

  connectSignals appRef

  widgetShowAll (mainWindow hs)

  args <- getArgs
  when ((length args) > 0) $ do
    let file = head args
    exists <- doesFileExist file
    if exists
      then openFile appRef file
      else do 
        dir <- getCurrentDirectory
        let file' = dir ++ "/" ++ file
        exists' <- doesFileExist file'
        if exists' 
          then openFile appRef file'
          else putStrLn "Could not find file, ignoring..."

  timeoutId <- timeoutAdd (updateUI appRef) 50

  mainGUI

  timeoutRemove timeoutId
  
  putStrLn "Closing..."
  app <- readIORef appRef
  when (isJust $ appPlayer app) $ do 
    let p = fromJust $ appPlayer app
    mpvStop p
    mpvTerminate p
    

