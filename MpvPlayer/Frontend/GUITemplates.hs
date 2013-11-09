{-# LANGUAGE TemplateHaskell #-}

module MpvPlayer.Frontend.GUITemplates (
  module MpvPlayer.Frontend.GUITemplates,
  module Graphics.UI.Gtk,
  module Graphics.UI.Gtk.Builder,
  module Graphics.UI.Gtk.Windows.Window,
  module Graphics.UI.Gtk.MenuComboToolbar.ToolButton,
  module Graphics.UI.Gtk.MenuComboToolbar.MenuToolButton,
  module Graphics.UI.Gtk.MenuComboToolbar.ToggleToolButton,
  module Graphics.UI.Gtk.Entry.HScale,
  module Graphics.UI.Gtk.Buttons.VolumeButton,
  module Graphics.UI.Gtk.Display.Statusbar,
  module Graphics.UI.Gtk.Misc.DrawingArea,
  module Graphics.UI.Gtk.Layout.VBox
) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Windows.Window
import Graphics.UI.Gtk.MenuComboToolbar.ToolButton
import Graphics.UI.Gtk.MenuComboToolbar.MenuToolButton
import Graphics.UI.Gtk.MenuComboToolbar.ToggleToolButton
import Graphics.UI.Gtk.Entry.HScale
import Graphics.UI.Gtk.Buttons.VolumeButton
import Graphics.UI.Gtk.Display.Statusbar
import Graphics.UI.Gtk.Misc.DrawingArea
import Graphics.UI.Gtk.Layout.VBox
import Graphics.UI.Gtk.Display.Image

import Control.Monad
import Data.Char
import Data.List

import Language.Haskell.TH

data Handles = Handles {
      mainWindow        :: Window,
      mainBox           :: VBox,
      background        :: EventBox,
      backgroundImage   :: Image,
      openButton        :: ToolButton,
      settingsButton    :: MenuToolButton,
      aboutButton       :: MenuToolButton,
      playButton        :: ToggleToolButton,
      scale             :: HScale,
      volumeButton      :: VolumeButton,
      statusbar         :: Statusbar,
      aboutDialog       :: AboutDialog,
      settingsDialog    :: Dialog,
      cmdLineEntry      :: Entry
} 

getHandlesExp :: Q Exp -> Q Exp
getHandlesExp builder = do
  (TyConI (DataD _ _ _ [RecC _ vars] _)) <- reify ''Handles
  objs <- forM vars $ \(n,_,t) -> do
            let widgetName' = (toUpper $ head $ nameBase n):(tail $ nameBase n)
            let tStr = reverse $ takeWhile (/= '.') $ reverse $ show t
            let castStr = "castTo" ++ tStr
            
            (Just castTo) <- lookupValueName castStr
                        
            return (widgetName', castTo, t)

  let bind n w castTo = bindS (varP $ mkName n) 
                      [| builderGetObject $builder $(varE castTo) w |]
  let stmts = map (\(w,castTo,_) -> bind ("var" ++ w) w castTo) objs

  let retExp = foldl' appE [| Handles |] (map (\(w,_,_) -> dyn ("var" ++ w)) objs)
  doE (stmts ++ [ noBindS [| return $(retExp) |] ])
  
  
