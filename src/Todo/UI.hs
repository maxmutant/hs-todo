{-# LANGUAGE TemplateHaskell #-}

-- --------------------------------------------------------------------------
-- |
-- Module      :  Todo.UI
-- Copyright   :  (c) Maximilian Mayer 2020
-- License     :  MIT (see LICENSE)
--
-- Maintainer  :  max@maxmayer.xyz
-- Stability   :
-- Portability :
--
-- This module provides rendering and handling of user interaction.
--
-----------------------------------------------------------------------------

module Todo.UI
( runMain
) where

import Todo.FixedStrings
import Todo.IO
import Todo.Item

import Brick.Util (bg, fg, on)
import Brick.Widgets.Core
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Char
import Data.Text.Zipper (clearZipper, insertMany)
import Lens.Micro
import Lens.Micro.TH
import Text.Printf (printf)

import qualified Brick.AttrMap      as BA
import qualified Brick.Focus        as BF
import qualified Brick.Main         as BM
import qualified Brick.Types        as BT
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.List as BL
import qualified Data.Vector        as Vec
import qualified Graphics.Vty       as V

-- | Name, the definition of the 2 widgets used
data Name
    = List -- ^ The list of TodoItems
    | Edit -- ^ Bottom editor for text manipulation
    deriving (Eq, Ord, Show)

-- | EditMode, the representation of "what the user is currently editing"
data EditMode
    = None
    | AddNew
    | EditEntry
    | SearchEntry
    | ExitPrompt

instance Show EditMode where
    show None        = txtLNone
    show AddNew      = txtLAdd
    show EditEntry   = txtLEdit
    show SearchEntry = txtLSearch
    show ExitPrompt  = txtLExit

-- | St, the current state of the application
data St = St
    { _file      :: String                -- ^ Path to the used todo.txt file
    , _focusRing :: BF.FocusRing Name     -- ^ Represents focused widget
    , _list      :: BL.List Name TodoItem -- ^ List of all TodoItems
    , _label     :: String                -- ^ Text "in front of" editor
    , _edit      :: BE.Editor String Name -- ^ Editor for text manipulation
    , _editMode  :: EditMode              -- ^ What is user currently editing
    , _nextId    :: Integer               -- ^ Next valid id for new item
    , _sorting   :: Maybe SortFunc
    }
makeLenses ''St

initialState :: String -> [TodoItem] -> St
initialState path todoItems =
    St path
       (BF.focusRing [List, Edit])
       (BL.list List (Vec.fromList todoItems) 1)
       ""
       (BE.editor Edit (Just 1) "")
       None
       (toInteger $ length todoItems + 1)
       Nothing

-- --------------------------------------------------------------------------
-- | The main entry point of the brick application. The path to the used
-- todo.txt file and all its content as a list of parsed TodoItems must
-- be provided.
--
-- Note: The brick application does not return anything, as everything
-- (including handling of unsaved changes and input errors) is managed
-- inside the brick app. Therefore, returning any exit code wouldn't
-- make sense.
runMain :: String -> [TodoItem] -> IO ()
runMain path entries = void $ BM.defaultMain todoApp $ initialState path entries

-- --------------------------------------------------------------------------
-- Base app definition

todoApp :: BM.App St e Name
todoApp = BM.App { BM.appDraw         = drawUI
                 , BM.appChooseCursor = BM.showFirstCursor
                 , BM.appHandleEvent  = appHandleEvent
                 , BM.appStartEvent   = return
                 , BM.appAttrMap      = const appAttrMap
                 }

-- --------------------------------------------------------------------------
-- Attributes

appAttrMap :: BA.AttrMap
appAttrMap = BA.attrMap V.defAttr
    [ (BL.listAttr,            V.white `on` V.black)
    , (BL.listSelectedAttr,    bg V.yellow)
    , (selectedAttr,           fg V.black)
    , (contextAttr,            fg V.green)
    , (doneAttr,               fg V.brightBlack)
    , (priorityAttr,           fg V.magenta)
    , (projectAttr,            fg V.blue)
    ]

selectedAttr, contextAttr, doneAttr, priorityAttr, projectAttr :: BA.AttrName
selectedAttr = BA.attrName "selected"
contextAttr  = BA.attrName "context"
doneAttr     = BA.attrName "done"
priorityAttr = BA.attrName "priority"
projectAttr  = BA.attrName "project"

-- --------------------------------------------------------------------------
-- Drawing

drawUI :: St -> [BT.Widget Name]
drawUI st = [ui]
    where
        f = st^.focusRing
        l = st^.list
        e = st^.edit
        la = if null (st^.label)
                then str ""
                else str (st^.label) <+> str ": "
        cur = case l^.BL.listSelectedL of
                Nothing -> "-"
                Just i  -> show (i + 1)
        total = show $ Vec.length $ l^.BL.listElementsL
        header = str $ printf txtHeader cur total
        footer = la <+> BF.withFocusRing f (BE.renderEditor (str. unlines)) e
        ui = vBox [ header
                  , BF.withFocusRing f (BL.renderList listDrawElement) l
                  , footer
                  ]

-- | Draw a TodoItem and highlight its individual parts.
-- Note: This does not retain the items original internal order.
listDrawElement :: Bool -> TodoItem -> BT.Widget Name
listDrawElement isSelected item = limit $ itemString <+> fill ' '
    where
        limit = hLimitPercent 100 . vLimit 1

        itemString = foregroundOverride
                  $ spaceIndent (printDone item)
                  <+> withAttr priorityAttr (str (printPriority item))
                  <+> str (printDate item ++ printDescription item)
                  <+> withAttr projectAttr (spaceIndent (printProjects item))
                  <+> withAttr contextAttr (spaceIndent (printContexts item))
                  <+> spaceIndent (printKeyVals item)

        -- | Forcefully override the foreground color of a TodoItem.
        -- This discards the priority, project and context highlights.
        foregroundOverride :: BT.Widget n -> BT.Widget n
        foregroundOverride
          | isDone item = forceAttr doneAttr
          | isSelected  = forceAttr selectedAttr
          | otherwise   = id

        spaceIndent :: String -> BT.Widget n
        spaceIndent "" = emptyWidget
        spaceIndent s  = str $ ' ' : s

-- --------------------------------------------------------------------------
-- Event handling

appHandleEvent :: St -> BT.BrickEvent Name e -> BT.EventM Name (BT.Next St)
appHandleEvent st (BT.VtyEvent e) =
    case BF.focusGetCurrent (st^.focusRing) of
      Just List -> case e of
            V.EvKey V.KEsc [] -> onExit st
            V.EvKey (V.KChar 'q') [] -> onExit st
            V.EvKey (V.KChar 'w') [] -> BM.continue =<< liftIO (onWriteFile st)
            V.EvKey (V.KChar '/') [] -> BM.continue $ enterEdit st SearchEntry
            V.EvKey (V.KChar ' ') [] -> BM.continue $ onToggleDone st
            V.EvKey (V.KChar 'n') [] -> BM.continue $ enterEdit st AddNew
            V.EvKey (V.KChar 'x') [] -> BM.continue $ listItemDelete st
            V.EvKey (V.KChar 'e') [] -> BM.continue $ onEditCurrent st
            V.EvKey (V.KChar 's') [] -> BM.continue $ resort sortFull st
            V.EvKey (V.KChar '1') [] -> BM.continue $ resort sortDone st
            V.EvKey (V.KChar '2') [] -> BM.continue $ resort sortDate st
            V.EvKey (V.KChar '3') [] -> BM.continue $ resort sortPrio st
            V.EvKey (V.KChar '4') [] -> BM.continue $ resort sortProj st
            V.EvKey (V.KChar '5') [] -> BM.continue $ resort sortCont st
            V.EvKey (V.KChar '=') [] -> BM.continue $ unsort st
            _ -> BM.continue =<< listDefaultHandler st e
      Just Edit -> case e of
            V.EvKey V.KEsc []   -> BM.continue . clearEdit $ leaveEdit st
            V.EvKey V.KEnter [] -> onProcessInput st
            _ -> BM.continue =<<  editDefaultHandler st e
      Nothing -> BM.continue st
appHandleEvent st _ = BM.continue st

listDefaultHandler :: St -> V.Event -> BT.EventM Name St
listDefaultHandler st = BT.handleEventLensed st list viHandler
    where viHandler = BL.handleListEventVi BL.handleListEvent

editDefaultHandler :: St -> V.Event -> BT.EventM Name St
editDefaultHandler st = BT.handleEventLensed st edit BE.handleEditorEvent

-- | Before closing, check if there are unsaved changes and
-- ask user what to do.
onExit :: St -> BT.EventM Name (BT.Next St)
onExit st = do
    savedItems <- liftIO (readTodoFile (st^.file))
    if length current /= length savedItems || differ current savedItems
       then BM.continue $ enterEdit st ExitPrompt
       else BM.halt st
    where
        current = unsortedItems st

        differ :: [TodoItem] -> [TodoItem] -> Bool
        differ x y = elem False $ zipWith compareByText x y

onWriteFile :: St -> IO St
onWriteFile st = do
    let path = st^.file
        items = unsortedItems st
        msg = printf txtSave (st^.file)
    writeTodoFile path items
    return $ setEditText msg st

onToggleDone :: St -> St
onToggleDone st = redoLastSort $ st & list %~ BL.listModify toggleDone

onEditCurrent :: St -> St
onEditCurrent st =
    case BL.listSelectedElement (st^.list) of
      Nothing        -> setEditText txtErrNoSel st
      Just (_, item) -> setEditText (show item) $ enterEdit st EditEntry

onProcessInput :: St -> BT.EventM Name (BT.Next St)
onProcessInput st =
    case st^.editMode of
      AddNew     -> backToList $ listItemAdd st
      EditEntry  -> backToList $ listItemUpdate st
      ExitPrompt -> handleUnsavedChanges st
      _          -> BM.continue st
    where
        backToList = BM.continue . redoLastSort . leaveEdit


handleUnsavedChanges :: St -> BT.EventM Name (BT.Next St)
handleUnsavedChanges st
    | i == "y" || i == "yes" = BM.halt =<< liftIO (onWriteFile st)
    | i == "n" || i == "no"  = BM.halt st
    | otherwise              = BM.continue . clearEdit $ leaveEdit st
    where input = BE.getEditContents (st^.edit)
          i     = if null input then "" else map toLower $ head input

-- --------------------------------------------------------------------------
-- Editor operations

parseValidInput :: [String] -> Either String TodoItem
parseValidInput input =
    if null input || null (head input)
       then Left txtErrEmpty
       else parseTodoItem $ head input

enterEdit :: St -> EditMode -> St
enterEdit st m = st & clearEdit
                    & focusRing %~ BF.focusSetCurrent Edit
                    & editMode .~ m
                    & label .~ show m

leaveEdit :: St -> St
leaveEdit st = st & focusRing %~ BF.focusSetCurrent List
                  & editMode .~ None
                  & label .~ ""

clearEdit :: St -> St
clearEdit st = st & edit %~ BE.applyEdit clearZipper

setEditText :: String -> St -> St
setEditText s st = st & clearEdit & edit %~ BE.applyEdit (insertMany s)

-- --------------------------------------------------------------------------
-- List manipulation

listItemAdd :: St -> St
listItemAdd st =
    case parseValidInput $ BE.getEditContents (st^.edit) of
      Left err       -> setEditText err st
      Right todoItem -> setEditText txtAdd . incrementId . select $ insert st
          where
              insert = list %~ BL.listInsert position itemWithId
              select = list %~ BL.listMoveTo position
              incrementId  = nextId %~ (+1)

              position = nextPosition st
              itemWithId = setId todoItem (st^.nextId)

              nextPosition :: St -> Int
              nextPosition s = Vec.length $ s ^. (list . BL.listElementsL)

listItemUpdate :: St -> St
listItemUpdate st =
    case parseValidInput $ BE.getEditContents (st^.edit) of
      Left err       -> setEditText err st
      Right todoItem -> setEditText txtEdit $ update todoItem st
          where
              update item = list %~ BL.listModify (replaceWith item)

              replaceWith :: TodoItem -> TodoItem -> TodoItem
              replaceWith new old = setId new $ getId old

listItemDelete :: St -> St
listItemDelete st =
    case st ^. (list . BL.listSelectedL) of
      Nothing -> setEditText txtErrNoSel st
      Just i  -> setEditText txtDel $ delete i st
          where
              delete item = list %~ BL.listRemove item

-- --------------------------------------------------------------------------
-- Sorting

-- | Wrapper to sort list within state based on provided function.
-- Basic sorting functions are defined in Todo.Item
applySort :: SortFunc -> St -> St
applySort f st = do
    let l = st^.list
        sorted = l & BL.listElementsL %~ f
    st & list .~ case BL.listSelectedElement l of
                   Nothing -> sorted
                   Just (_, i) -> BL.listMoveToElement i sorted
       & sorting ?~ f

-- | Unsort to original form (order of items within file)
unsort :: St -> St
unsort = applySort sortId

-- | Same as unsort, but returns a list instead of a new state
unsortedItems :: St -> [TodoItem]
unsortedItems st = Vec.toList $ ust ^. (list . BL.listElementsL)
    where ust = unsort st

-- | Sort list based on given function, whereby prior sorts
-- are discarded.
resort :: SortFunc -> St -> St
resort f = applySort f . unsort

redoLastSort :: St -> St
redoLastSort st = case st^.sorting of
                    Nothing -> st
                    Just f  -> resort f st
