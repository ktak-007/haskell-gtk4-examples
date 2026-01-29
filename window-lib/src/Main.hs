{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Actions
import           Menu
import           Tree
import           Window

-- haskell-gi-base
import           Data.GI.Base

-- gi-gtk
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
  runApplicationWindow
    ApplicationProperties
      { applicationId = "org.gtk.window-lib-example"
      , menu = [ Section Nothing
                  [ "New" ==> FILE_NEW
                  , "Open" ==> FILE_OPEN
                  , "Save" ==> FILE_SAVE
                  , "Save as..." ==> FILE_SAVE_AS
                  , "Change password" ==> FILE_CHANGE_PASSWORD
                  ]
               , Section Nothing
                  [ "Say Hello" ==> APP_SAY_HELLO
                  , "Lock" ==> APP_LOCK
                  , "Exit" ==> APP_QUIT
                  ]
               ]
      , actions = [ APP_SAY_HELLO >== \_ -> putStrLn "Hello from menu!"
                  , APP_QUIT >== \app -> app.quit
                  ]
      , sidebar = SidebarPage { title = "Menu"
                              , content = Just $ unsafeCastTo Gtk.Widget =<< getTreeView
                              }
      , content = ContentPage { title = "Libraries Code Example"
                              , subtitle = Just "Test program"
                              , content = Nothing
                              }
      }
