{-# LANGUAGE TemplateHaskell #-}

module Main where

import MpvPlayer.Frontend.GUITemplates
import MpvPlayer.Backend

import Data.IORef
import Data.Maybe
import Data.Time.Format
import Control.Monad

data App = App {
      appHandles :: Handles,
      appPlayer  :: Maybe (IORef Player), 
      appToggleSigId :: Maybe (ConnectId ToggleToolButton),
      appVolSigId :: Maybe (ConnectId VolumeButton),
      appStatusContextId :: ContextId
}

openFile :: IORef App -> FilePath -> IO ()
openFile appRef filename = do
  app <- readIORef appRef
  drawWin <- widgetGetDrawWindow $ videoArea $ appHandles app
  wid <- liftM fromNativeWindowId $ drawableGetID drawWin
  playerRef <- mpvPlay wid filename ()
  writeIORef appRef (app { appPlayer = Just playerRef })

showOpenDialog :: IORef App -> IO ()
showOpenDialog appRef = do
  app <- readIORef appRef
  dialog <- fileChooserDialogNew Nothing 
            (Just $ mainWindow $ appHandles app) FileChooserActionOpen []
  dialogAddButton dialog "gtk-cancel" $ ResponseUser 0
  dialogAddButton dialog "gtk-open" $ ResponseUser 1
  (ResponseUser resp) <- dialogRun dialog
  
  when (resp == 1) $ do file <- fileChooserGetFilename dialog
                        case file of
                          Just fn -> openFile appRef fn
                          Nothing -> return ()
 
  widgetDestroy dialog

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

connectSignals :: IORef App -> IO ()
connectSignals appRef = do
  app <- readIORef appRef
  let hs = appHandles app

  onToolButtonClicked (openButton hs) $ showOpenDialog appRef
  idPlay <- afterToolButtonToggled (playButton hs) $ playToggle appRef
                      
  onAdjustBounds (scale hs) $ seek appRef

  idVol <- on (volumeButton hs) scaleButtonValueChanged $ setVolume appRef

  onDestroy (mainWindow hs) mainQuit
  
  writeIORef appRef app { appToggleSigId = Just idPlay,
                          appVolSigId = Just idVol }

prepareUI :: Handles -> IO Handles
prepareUI hs = do
  -- TODO: show black background for video area

  -- HACK: volume button shows no icon when loaded from Glade.
  p <- widgetGetParent (volumeButton hs)
  let box = castToBox $ fromJust p
  widgetDestroy (volumeButton hs)
  volBut <- volumeButtonNew
  boxPackEnd box volBut PackNatural 0

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

updateUI :: IORef App -> IO Bool
updateUI appRef = do
  app <- readIORef appRef
  let hs = appHandles app

  signalBlock $ fromJust $ appToggleSigId app
  signalBlock $ fromJust $ appVolSigId app

  if (isJust $ appPlayer app)
     then do
       widgetSetSensitive (scale hs) True
       widgetSetSensitive (playButton hs) True

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
         
     else do 
       rangeSetValue (scale hs) 0.0
       widgetSetSensitive (scale hs) False 
       set (playButton hs) [toggleToolButtonActive := False]
       widgetSetSensitive (playButton hs) False
       statusbarRemoveAll (statusbar hs) (fromIntegral $ appStatusContextId app)

  signalUnblock $ fromJust $ appToggleSigId app
  signalUnblock $ fromJust $ appVolSigId app
                 
  return True

main :: IO ()
main = do
  initGUI
  builder <- builderNew
  builderAddFromFile builder "main-gtk2.ui"
  hs <- prepareUI =<< $(getHandlesExp [| builder |])

  statusId <- statusbarGetContextId (statusbar hs) "SimpleStatus" 

  appRef <- newIORef $ App hs Nothing Nothing Nothing statusId

  connectSignals appRef

  widgetShowAll (mainWindow hs)

  timeoutAdd (updateUI appRef) 50

  mainGUI
  
  putStrLn "Closing..."
  app <- readIORef appRef
  when (isJust $ appPlayer app) $ do 
    let p = fromJust $ appPlayer app
    mpvStop p
    mpvTerminate p
    

