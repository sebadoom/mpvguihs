{-# LANGUAGE TemplateHaskell #-}

module Main where

import MpvPlayer.Frontend.GUITemplates
import MpvPlayer.Backend

import Data.IORef
import Data.Maybe
import Control.Monad
import Control.Concurrent

import Graphics.UI.Gtk.Selectors.FileChooserDialog
import Graphics.UI.Gtk.Selectors.FileChooser

data App = App {
      appHandles :: Handles,
      appPlayer  :: Maybe (IORef Player)
}

volumeChanged :: IORef App -> Double -> IO ()
volumeChanged appRef value = return ()

openFile :: IORef App -> FilePath -> IO ()
openFile appRef filename = do
  app <- readIORef appRef
  drawWin <- widgetGetDrawWindow $ videoArea $ appHandles app
  wid <- liftM fromNativeWindowId $ drawableGetID drawWin
  playerRef <- mpvPlay wid filename ()
  toggleToolButtonSetActive (playButton $ appHandles app) True
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
  app <- readIORef appRef
  pressed <- toggleToolButtonGetActive (playButton $ appHandles app)
  
  case (appPlayer app) of
    Just p -> if pressed 
              then mpvPause p
              else mpvUnpause p
    Nothing -> return ()

seek :: IORef App -> Double -> IO ()
seek appRef value = do
  app <- readIORef appRef
  when (isJust $ appPlayer app) $ 
    mpvSeek (fromJust $ appPlayer app) value

connectSignals :: IORef App -> IO ()
connectSignals appRef = do
  app <- readIORef appRef
  let hs = appHandles app

  onToolButtonClicked (openButton hs) $ showOpenDialog appRef
  onToolButtonToggled (playButton hs) $ playToggle appRef
                      
  onAdjustBounds (scale hs) $ seek appRef

  onDestroy (mainWindow hs) mainQuit
  return ()

prepareUI hs = do
  -- TODO: show black background for video area
  return ()

updateUI :: IORef App -> IO Bool
updateUI appRef = do
  app <- readIORef appRef
  let hs = appHandles app
  when (isJust $ appPlayer app) $ do
    let p = fromJust $ appPlayer app
    status <- mpvGetPlayStatus p
    case status of
      Nothing -> return ()
      Just s  -> do
        let ratio = (playStatusPos s) / (playStatusLength s)
        rangeSetValue (scale hs) ratio
                 
  return True

main = do
  initGUI
  builder <- builderNew
  builderAddFromFile builder "main-gtk2.ui"
  hs <- $(getHandlesExp [| builder |])

  appRef <- newIORef $ App hs Nothing

  connectSignals appRef
  prepareUI hs
  widgetShowAll (mainWindow hs)

  idleAdd (updateUI appRef) priorityDefaultIdle

  mainGUI
  
  putStrLn "Closing..."
  app <- readIORef appRef
  when (isJust $ appPlayer app) $ do 
    let p = fromJust $ appPlayer app
    mpvStop p
    mpvTerminate p
    

