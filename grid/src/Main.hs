{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getProgName, getArgs)
import System.Exit (exitWith, ExitCode (ExitFailure, ExitSuccess))

-- haskell-gi-base
import Data.GI.Base

-- text
import Data.Text (Text)

-- gi-gtk
import qualified GI.Gtk as Gtk

appId :: Text
appId = "org.example.Test"

main :: IO ()
main = do
  app <- new Gtk.Application
    [ #applicationId := appId
    , On #activate $ buildUi ?self
    ]

  progName <- getProgName
  args <- getArgs

  app.run (Just $ progName : args) >>= \status ->
    exitWith $ if status == 0
               then ExitSuccess
               else ExitFailure $ fromIntegral status

buildUi :: Gtk.Application -> IO ()
buildUi app = do
  grid <- new Gtk.Grid
    [ #rowSpacing := 12
    , #columnSpacing := 12
    , #columnHomogeneous := True
    , #rowHomogeneous := True
    ]

  window <- new Gtk.ApplicationWindow
    [ #application  := app
    , #title        := "Grid Example"
    , #defaultWidth := 400
    , #child := grid
    ]
  label <- withDefaults =<< newBoldLabel "Hello, World!"
  grid.attach label 0 0 2 1

  button1 <- withDefaults =<< new Gtk.Button
    [ #label         := "Press me!"
    , On #clicked $ setLabelBold label "Button clicked!"
    ]
  grid.attach button1 0 1 1 1

  button2 <- withDefaults =<< new Gtk.Button
    [ #label         := "Don't press me!"
    , On #clicked $ setLabelBold label "Boo!"
    ]
  grid.attach button2 1 1 1 1

  quitButton <- withDefaults =<< new Gtk.Button
    [ #label         := "Exit"
    , On #clicked $ #destroy window
    ]
  grid.attach quitButton 0 2 2 1

  Gtk.windowPresent window

-- Helpers:

withDefaults :: Gtk.IsWidget w => w -> IO w
withDefaults widget = do
  widget' <- Gtk.toWidget widget
  set widget'
    [ #marginTop    := 12
    , #marginBottom := 12
    , #marginStart  := 12
    , #marginEnd    := 12
    , #hexpand      := False
    , #vexpand      := False
    , #halign       := Gtk.AlignCenter
    , #valign       := Gtk.AlignCenter
    ]
  return widget

newBoldLabel :: Text -> IO Gtk.Label
newBoldLabel text = do
  label <- Gtk.labelNew Nothing
  setLabelBold label text
  Gtk.set label [ #halign := Gtk.AlignCenter
                , #valign := Gtk.AlignCenter
                ]
  return label

setLabelBold :: Gtk.Label -> Text -> IO ()
setLabelBold label text =
  Gtk.labelSetMarkup label $ "<span font='20' weight='bold'>" <> text <> "</span>"
