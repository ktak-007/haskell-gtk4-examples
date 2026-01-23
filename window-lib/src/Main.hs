{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Actions
import           Menu
import           Window

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
      , sidebar = NavigationPage { title = "Menu"
                                 , content = Nothing
                                 }
      }
