{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ImplicitParams #-}

module Main where

import           Control.Monad (void)
import           Data.Text (Text, pack)
import           System.Environment (getArgs, getProgName)

-- haskell-gi-base
import           Data.GI.Base

-- gi-adwaita
import qualified GI.Adw as Adw

-- gi-gio
import qualified GI.Gio as Gio

-- gi-gtk
import qualified GI.Gtk as Gtk

data Action = FILE_NEW
            | FILE_OPEN
            | FILE_SAVE
            | FILE_SAVE_AS
            | FILE_CHANGE_PASSWORD
            | APP_SAY_HELLO
            | APP_LOCK
            | APP_QUIT
            deriving (Show)

main :: IO ()
main = do
  app <- new Adw.Application [ #applicationId := "org.gtk.window-split-example"
                             , On #activate (activate ?self)
                             ]
  args <- getArgs
  progName <- getProgName
  -- void $ app.run $ Just $ progName : args
  void $ app.run $ Nothing

activate :: Adw.Application -> IO ()
activate app = do
  sidebarView <- new Adw.ToolbarView [
    -- #content :=> sidebarContent
    ]
  sidebarView.addTopBar =<< titlebarLeft
  sidebarPage <- new Adw.NavigationPage [ #child := sidebarView, #title := "Menu" ]

  contentView <- new Adw.ToolbarView []
  contentView.addTopBar =<< titlebarRight
  contentPage <- new Adw.NavigationPage [ #child := contentView, #title := "Content" ]

  splitView <- new Adw.NavigationSplitView [ #sidebar := sidebarPage, #content := contentPage ]

  window <- new Adw.ApplicationWindow [ #application := app
                                      , #content :=> new Adw.ToolbarView [ #content := splitView ]
                                      , #defaultWidth := 600
                                      , #defaultHeight := 800
                                      ]
  initActions app

  window.present

titlebarLeft :: IO Adw.HeaderBar
titlebarLeft = do
  headerBar <- new Adw.HeaderBar []

  menu <- mainMenu

  headerBar.packStart =<< new Gtk.MenuButton [ #iconName := "open-menu-symbolic"
                                             , #menuModel := menu
                                             ]
  return headerBar

titlebarRight :: IO Adw.HeaderBar
titlebarRight = do
  title <- new Adw.WindowTitle [ #title := "Window Split Example"
                               , #subtitle := "Test Program"
                               ]
  new Adw.HeaderBar [ #titleWidget := title ]

mainMenu :: IO Gio.Menu
mainMenu = do
  menu <- new Gio.Menu []

  fileMenu <- new Gio.Menu []
  mapM_ (append fileMenu)
    [ "New" ==> FILE_NEW
    , "Open" ==> FILE_OPEN
    , "Save" ==> FILE_SAVE
    , "Save as..." ==> FILE_SAVE_AS
    , "Change password" ==> FILE_CHANGE_PASSWORD
    ]
  menu.appendSection Nothing fileMenu

  appMenu <- new Gio.Menu []
  mapM_ (append appMenu)
    [ "Say Hello" ==> APP_SAY_HELLO
    , "Lock" ==> APP_LOCK
    , "Exit" ==> APP_QUIT
    ]
  menu.appendSection Nothing appMenu

  return menu

  where append :: Gio.Menu -> (Text, Action) -> IO ()
        append m (text, action) = m.append (Just text) (Just $ "app." <> showt action)

initActions :: Adw.Application -> IO ()
initActions app = do
  createAction APP_SAY_HELLO $ putStrLn "Hello from menu!"
  createAction APP_QUIT app.quit

  where
  createAction :: Action -> IO () -> IO ()
  createAction action callback = do
    action' <- Gio.simpleActionNew (showt action) Nothing
    on action' #activate $ const callback
    app.addAction action'

showt :: Show a => a -> Text
showt o = pack $ show o

(==>) :: Text -> Action -> (Text, Action)
(==>) a b = (a, b)
