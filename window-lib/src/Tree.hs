{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Tree ( getTreeView ) where

import           Data.Foldable ( for_, traverse_ )
import           Data.Maybe ( fromJust )
-- import           Debug.Trace (traceIO)

-- haskell-gi-base
import           Data.GI.Base
import           Data.GI.Base.GObject
import           Data.GI.Base.ShortPrelude ( whenJust )
import           Data.GI.Base.Overloading

-- gi-gtk
import qualified GI.Gtk as Gtk

-- gi-gio
import qualified GI.Gio as Gio

-- gi-gobject
import qualified GI.GObject as GObject
import GHC.Stack (HasCallStack)
import Data.Text (Text)

-- Useful data to show
data TreeNode = TreeNode
  { label :: Text
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

-- | Custom Gtk object used to hold layer data.
--
-- These are the items that will get stored in the ListModel used by Gtk
-- to store the layer hierarchy data.
newtype TreeNodeItem = TreeNodeItem ( Gtk.ManagedPtr TreeNodeItem )

instance TypedObject TreeNodeItem  where
  glibType = registerGType TreeNodeItem

instance GObject TreeNodeItem

instance HasParentTypes TreeNodeItem
type instance ParentTypes TreeNodeItem = '[ GObject.Object ]

instance DerivedGObject TreeNodeItem where
  type GObjectParentType  TreeNodeItem = GObject.Object
  type GObjectPrivateData TreeNodeItem = Maybe TreeNode
  objectTypeName = "gtk-layers-TreeNodeItem"
  objectClassInit _ = return ()
  objectInstanceInit _ _ = return Nothing
  objectInterfaces = [ ]

makeListStore :: [TreeNode] -> IO Gio.ListStore
makeListStore nodes = do
  store <- Gio.listStoreNew =<< glibType @TreeNodeItem
  for_ nodes $ \node -> do
    item <- unsafeCastTo TreeNodeItem =<< new TreeNodeItem []
    gobjectSetPrivateData item ( Just node )
    Gio.listStoreAppend store item

  return store

-- getChildrenFunc :: Gtk.TreeListModelCreateModelFunc
getChildrenFunc :: GObject.Object -> IO ( Maybe Gio.ListModel )
getChildrenFunc parent = do
  node <- getLayerData =<< unsafeCastTo TreeNodeItem parent
  if null $ children node
  then return Nothing
  else do
    childStore <- Gio.listStoreNew =<< glibType @TreeNodeItem
    for_ (children node) $ \child -> do
      item <- unsafeCastTo TreeNodeItem =<< new TreeNodeItem []
      gobjectSetPrivateData item ( Just child )
      Gio.listStoreAppend childStore item
    childListModel <- Gio.toListModel childStore
    return $ Just childListModel

getTreeView :: IO Gtk.ListView
getTreeView = do
  rootModel <- Gio.toListModel =<< makeListStore treeData

  treeModel <- Gtk.treeListModelNew
    rootModel
    False                      -- Must not use passthrough to use TreeExpander widgets.
    False                      -- Autoexpand on creation; we later set this to False.
    getChildrenFunc

  factory <- Gtk.signalListItemFactoryNew

  on factory #setup $ \item -> do
    listItem <- unsafeCastTo Gtk.ListItem item
    Gtk.listItemSetFocusable listItem False

    expander <- Gtk.treeExpanderNew

    Gtk.treeExpanderSetIndentForIcon  expander True
    Gtk.treeExpanderSetIndentForDepth expander True
    Gtk.treeExpanderSetHideExpander   expander False

    contentBox <- Gtk.boxNew Gtk.OrientationHorizontal 20
    Gtk.treeExpanderSetChild expander ( Just contentBox )

    label <- Gtk.labelNew Nothing
    Gtk.boxAppend contentBox label

    #setXalign label 0
    #setMarginStart label 20
    #setMarginTop label 4
    #setMarginBottom label 4
    #setChild listItem (Just expander)

  on factory #bind $ \item -> do
    listItem <- unsafeCastTo Gtk.ListItem item
    mbExpander <- #getChild listItem
    expander <- case mbExpander of
        Nothing -> error "getTreeView onBind: list item has no child"
        Just expander' -> unsafeCastTo Gtk.TreeExpander expander'
    mbContentBox <- Gtk.treeExpanderGetChild expander
    label <- case mbContentBox of
      Nothing -> error "getTreeView: expected ListItem->Expander->Box"
      Just contentBox0 -> do
        contentBox <- unsafeCastTo Gtk.Box contentBox0
        mbLabel <- traverse ( unsafeCastTo Gtk.Label ) =<< Gtk.widgetGetFirstChild contentBox
        case mbLabel of
          Nothing -> error "getLayerViewWidget: expected ListItem->Expander->Box->{CheckButton,LayerLabel}"
          Just label -> return label
    dat <- getLayerData listItem
    mbTreeListRow <- traverse ( unsafeCastTo Gtk.TreeListRow ) =<< Gtk.listItemGetItem listItem
    treeListRow <- case mbTreeListRow of
      Nothing -> error "newLayerView ListItem onBind: no TreeListRow"
      Just r -> return r
    Gtk.treeExpanderSetListRow expander ( Just treeListRow )
    Gtk.labelSetText label dat.label

  selectionModel <- new Gtk.SingleSelection [ #model := treeModel ]
  treeView <- new Gtk.ListView [ #model := selectionModel
                               , #factory := factory
                               ]
  -- Pass copies of (references to) the TreeListModel and SelectionModel,
  -- in order to retain ownership over them.
  -- selectionModel <- withManagedPtr treeModel $ \ lmPtr ->
  --                   withNewObject lmPtr $ \ lmCopy ->
  --                   new Gtk.SingleSelection [ #model := lmCopy ]
  -- treeView <- withManagedPtr selectionModel $ \ smPtr ->
  --             withNewObject smPtr $ \ smCopy ->
  --             new Gtk.ListView [ #model := smCopy
  --                              , #factory := factory
  --                              ]

  makeTreeViewTransparent treeView

  return treeView

-- | Class for objects which wrap a 'LayerItem'.
class HasLayerData a where
  -- | Get the layer data associated to a 'LayerItem' or object
  -- containing a 'LayerItem'.
  getLayerData :: HasCallStack => a -> IO TreeNode -- Item
instance HasLayerData TreeNodeItem where
  getLayerData item = do
    mbDat <- gobjectGetPrivateData item
    case mbDat of
      Nothing -> error "getLayerData: no private data"
      Just dat -> return dat
instance HasLayerData Gtk.TreeListRow where
  getLayerData row = do
    parLayerItem <- treeListRowLayerItem row
    getLayerData parLayerItem
instance HasLayerData Gtk.ListItem where
  getLayerData listItem = do
    layerItem <- treeListItemLayerItem listItem
    getLayerData layerItem

treeListItemLayerItem :: Gtk.ListItem -> IO TreeNodeItem
treeListItemLayerItem listItem = do
  mbListRow <- Gtk.listItemGetItem listItem
  case mbListRow of
    Nothing -> error "treeListItemLayerItem: ListItem has no item"
    Just listRow -> do
      treeListRowLayerItem =<< unsafeCastTo Gtk.TreeListRow listRow

treeListRowLayerItem :: Gtk.TreeListRow -> IO TreeNodeItem
treeListRowLayerItem listRow = do
  mbListRowItem <- Gtk.treeListRowGetItem listRow
  case mbListRowItem of
    Nothing   -> error "treeListRowLayerItem: TreeListRow has no item"
    Just item -> unsafeCastTo TreeNodeItem item

makeTreeViewTransparent :: Gtk.ListView -> IO ()
makeTreeViewTransparent treeView = do
  cssProvider <- new Gtk.CssProvider []
  cssClasses <- Gtk.getWidgetCssClasses treeView
  let makeTransparent cssClass = Gtk.cssProviderLoadFromString cssProvider
                               $ "." <> cssClass <> " { background-color: rgba(0, 0, 0, 0); }"
  whenJust cssClasses $ return $ traverse_ makeTransparent $ fromJust cssClasses
  styleContext <- treeView.getStyleContext
  styleContext.addProvider cssProvider $ fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION
  styleContext.addClass "transparent"

-- debugWidgetType :: Gtk.Widget -> IO ()
-- debugWidgetType w = do
--   gt <- gtypeFromInstance w
--   name <- gtypeName gt
--   traceIO ("[Gtk.Widget] real type = " <> name)
