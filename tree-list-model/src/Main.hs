-- Fill Gtk.TreeListModel
-- Ideas were taken from from gtk-layers: https://github.com/sheaf/gtk-layers
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Foldable (for_)
import Text.Show.Unicode (uprint)

import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio

-- haskell-gi-base
import qualified Data.GI.Base as GI
import qualified Data.GI.Base.GObject as GI
import qualified Data.GI.Base.Overloading as GI

-- gi-gobject
import qualified GI.GObject as GObject

-- Useful data to show
data TreeNode = TreeNode
  { label :: String
  , children :: [TreeNode]
  } deriving (Show)

treeData :: [TreeNode]
treeData =
  [ TreeNode "Животные"
      [ TreeNode "Млекопитающие"
          [ TreeNode "Кошки" []
          , TreeNode "Собаки" []
          ]
      , TreeNode "Птицы" []
      ]
  , TreeNode "Растения"
      [ TreeNode "Цветы" []
      , TreeNode "Деревья" []
      ]
  ]

-- | Custom GTK object used to hold layer data.
--
-- These are the items that will get stored in the ListModel used by GTK
-- to store the layer hierarchy data.
newtype TreeNodeItem = TreeNodeItem ( Gtk.ManagedPtr TreeNodeItem )

instance GI.TypedObject TreeNodeItem  where
  glibType = GI.registerGType TreeNodeItem

instance GI.GObject TreeNodeItem

instance GI.HasParentTypes TreeNodeItem
type instance GI.ParentTypes TreeNodeItem = '[ GObject.Object ]

instance GI.DerivedGObject TreeNodeItem where
  type GObjectParentType  TreeNodeItem = GObject.Object
  type GObjectPrivateData TreeNodeItem = Maybe TreeNode
  objectTypeName = "gtk-layers-TreeNodeItem"
  objectClassInit _ = return ()
  objectInstanceInit _ _ = return Nothing
  objectInterfaces = [ ]

main :: IO ()
main = do
  for_ treeData $ \node -> do
    uprint $ label node
  rootModel <- Gio.toListModel =<< makeListStore treeData

  treeModel <- Gtk.treeListModelNew
    rootModel
    True                        -- passthrough
    False                       -- autoexpand
    getChildrenFunc

  putStrLn "Hello!"

makeListStore :: [TreeNode] -> IO Gio.ListStore
makeListStore nodes = do
  store <- Gio.listStoreNew =<< GI.glibType @TreeNodeItem
  for_ nodes $ \node -> do
    item <- GI.unsafeCastTo TreeNodeItem =<< GI.new TreeNodeItem []
    GI.gobjectSetPrivateData item ( Just node )
    Gio.listStoreAppend store item

  return store

-- getChildrenFunc :: Gtk.TreeListModelCreateModelFunc
getChildrenFunc :: GObject.Object -> IO ( Maybe Gio.ListModel )
getChildrenFunc parent = do
  node <- getLayerData =<< Gtk.unsafeCastTo TreeNodeItem parent
  if null $ children node
  then return Nothing
  else do
    childStore <- Gio.listStoreNew =<< GI.glibType @TreeNodeItem
    for_ (children node) $ \child -> do
      item <- GI.unsafeCastTo TreeNodeItem =<< GI.new TreeNodeItem []
      GI.gobjectSetPrivateData item ( Just child )
      Gio.listStoreAppend childStore item

    return Nothing

  return Nothing
  where
  getLayerData item = do
    mbDat <- GI.gobjectGetPrivateData item
    case mbDat of
      Nothing -> error "getLayerData: no private data"
      Just dat -> return dat
