{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getProgName, getArgs)
import System.Exit (exitWith, ExitCode (ExitFailure, ExitSuccess))

-- haskell-gi-base
import Data.GI.Base

-- gi-gtk
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
  app <- new Gtk.Application
    [ #applicationId := "org.example.Test"
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
  window <- new Gtk.ApplicationWindow
    [ #application  := app
    , #title        := "Simple window example"
    , #defaultWidth := 400
    -- , #child := <some widget>
    ]
  Gtk.windowPresent window
