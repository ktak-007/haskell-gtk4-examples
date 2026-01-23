{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Window ( ApplicationProperties(..)
              , NavigationPage(..)
              , runApplicationWindow
              ) where

import           Actions
import           Menu

import           Control.Monad (void, when)
import           Data.Maybe (isJust, fromJust)
import           Data.Text (Text)

-- haskell-gi-base
import           Data.GI.Base

-- gi-adwaita
import qualified GI.Adw as Adw

-- gi-gio
import qualified GI.Gio as Gio

-- gi-gtk
import qualified GI.Gtk as Gtk

data NavigationPage = NavigationPage { title :: Text, content :: Maybe Gtk.Widget }

data ApplicationProperties = ApplicationProperties
  { applicationId :: Text
  , menu :: Menu
  , actions :: Actions
  , sidebar :: NavigationPage
  }

runApplicationWindow :: ApplicationProperties -> IO ()
runApplicationWindow props@ApplicationProperties {..} = do
  app <- new Adw.Application
    [ #applicationId := applicationId
    , On #activate (activate ?self props)
    ]
  void $ app.run $ Nothing

activate :: Adw.Application -> ApplicationProperties -> IO ()
activate app ApplicationProperties {..} = do
  sidebarView <- new Adw.ToolbarView [
    -- #content :=> sidebarContent
      -- #content := sidebar.content
    ]
  -- Adw.setToolbarViewContent
  when (isJust sidebar.content) $
    set sidebarView [ #content := fromJust sidebar.content ]
  sidebarView.addTopBar =<< titlebarLeft =<< createMenu menu
  sidebarPage <- new Adw.NavigationPage [ #child := sidebarView, #title := sidebar.title ]

  contentView <- new Adw.ToolbarView []
  contentView.addTopBar =<< titlebarRight
  contentPage <- new Adw.NavigationPage [ #child := contentView, #title := "Content" ]

  splitView <- new Adw.NavigationSplitView [ #sidebar := sidebarPage, #content := contentPage ]

  window <- new Adw.ApplicationWindow [ #application := app
                                      , #content :=> new Adw.ToolbarView [ #content := splitView ]
                                      , #defaultWidth := 600
                                      , #defaultHeight := 800
                                      ]
  initActions app actions

  window.present

titlebarLeft :: Gio.Menu -> IO Adw.HeaderBar
titlebarLeft menu = do
  headerBar <- new Adw.HeaderBar []

  headerBar.packStart =<< new Gtk.MenuButton [ #iconName := "open-menu-symbolic"
                                             , #menuModel := menu
                                             ]
  return headerBar

titlebarRight :: IO Adw.HeaderBar
titlebarRight = do
  title <- new Adw.WindowTitle [ #title := "Libraries Code Example"
                               , #subtitle := "Test Program"
                               ]
  new Adw.HeaderBar [ #titleWidget := title ]
