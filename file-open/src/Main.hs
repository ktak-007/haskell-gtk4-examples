{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

import qualified Control.Exception as E
import           Control.Monad (void)
import           System.Environment (getArgs, getProgName)

-- haskell-gi-base
import           Data.GI.Base
import           Data.GI.Base.Utils (whenJust)

-- gi-adwaita
import qualified GI.Adw as Adw

-- gi-gio
import qualified GI.Gio as Gio

-- gi-gtk
import qualified GI.Gtk as Gtk

activate :: Adw.Application -> IO ()
activate app = mdo
  content <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  title <- new Adw.WindowTitle [ #title := "Test" ]
  titlebar <- new Adw.HeaderBar [ #titleWidget := title ]
  #append content titlebar

  fd <- new Gtk.FileDialog []

  button <- new Gtk.Button
    [ #child :=> new Adw.ButtonContent [ #iconName := "document-open-symbolic"
                                       , #label := "Open file"
                                       ]
    , On #clicked $ do
      mbWindow <- Gtk.applicationGetActiveWindow app
      whenJust mbWindow
        $ \window' -> Gtk.fileDialogOpen fd
          (Just window')
          (Nothing :: Maybe Gio.Cancellable)
        $ Just (
          \_ aresult -> do
             result :: Either GError Gio.File <- E.try (Gtk.fileDialogOpenFinish fd aresult)
             case result of
               Left _err -> pure ()
               Right choice -> #getPath choice >>= print
             return ()
        )
    ]
  #append content button

  window <- new Adw.ApplicationWindow
    [ #application := app
    , #content := content
    , #defaultWidth := 400
    ]
  window.present

main :: IO ()
main = do
  app <- new Adw.Application
    [ #applicationId := "org.example.Test"
    , On #activate (activate ?self)
    ]

  args <- getArgs
  progName <- getProgName
  void (app.run $ Just $ progName : args)
