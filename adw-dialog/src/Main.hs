{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- gi-adwaita
import qualified GI.Adw as Adw

-- gi-gtk
import qualified GI.Gtk as Gtk

-- haskell-gi-base
import           Data.GI.Base

-- text
import           Data.Text (Text)

-- base
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitWith, ExitCode (ExitFailure, ExitSuccess))

appId :: Text
appId = "org.example.adw.dialog"

showAlertDialog :: Gtk.Window -> IO ()
showAlertDialog parent = do
  dialog <- new Adw.AlertDialog
      [ #heading := "Delete file"
      , #body := "Input the password to delete file:"
      , #defaultResponse := "cancel"
      , #closeResponse := "cancel"
      ]
  #addResponse dialog "cancel" "Cancel"
  #addResponse dialog "delete" "Delete"
  Adw.alertDialogSetResponseAppearance dialog "delete" Adw.ResponseAppearanceDestructive

  -- Password entry field
  entry <- new Gtk.Entry
    [ #marginStart := 20
    , #marginEnd := 20
    , #marginTop := 10
    , #marginBottom := 10
    , #visibility := False -- hidden symbols
    , #inputPurpose := Gtk.InputPurposePassword -- other password behaviour
    , #placeholderText := "Password"
    , #activatesDefault := True -- emit default action on Enter key
    ]
  Adw.alertDialogSetExtraChild dialog $ Just entry

  -- Activated by press Enter
  Gtk.onEntryActivate entry $ do
    buf <- Gtk.entryGetBuffer entry
    text <- Gtk.entryBufferGetText buf
    print $ "Hello: " <> text

  -- Activated on dialog's finish
  on dialog #response $ \responseId -> do
    print $ "You selectd " <> responseId
    buf <- Gtk.entryGetBuffer entry
    text <- Gtk.entryBufferGetText buf
    print $ "You entered: " <> text

  -- show
  Adw.dialogPresent dialog $ Just parent

  -- Focus to the entry field
  Gtk.widgetGrabFocus entry >> return ()

main :: IO ()
main = do
  app <- new Adw.Application
    [ #applicationId := appId
    , On #activate $ buildUi ?self
    ]

  progName <- getProgName
  args <- getArgs

  app.run (Just $ progName : args) >>= \status ->
    exitWith $ if status == 0
               then ExitSuccess
               else ExitFailure $ fromIntegral status

buildUi :: Adw.Application -> IO ()
buildUi app = do
  window <- new Gtk.Window
    [ #application  := app
    , #title := "Adw.AlertDialog Example"
    , #defaultWidth := 400
    , #defaultHeight := 300
    ]

  button <- new Gtk.Button
    [ #label := "Show dialog"
    , On #clicked $ showAlertDialog window
    ]

  box <- new Gtk.Box
    [ #orientation := Gtk.OrientationVertical
    , #spacing := 10
    ]
  #append box button
  Gtk.setWindowChild window box

  #present window
