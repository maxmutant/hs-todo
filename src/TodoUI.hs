{-# LANGUAGE TemplateHaskell #-}

module TodoUI
( runMain
) where

import Brick.Util (on)
import Brick.Widgets.Core (str, vBox, (<+>))
import Control.Monad.IO.Class (liftIO)
import Data.Text.Zipper
import Lens.Micro
import Lens.Micro.TH

import qualified Brick.AttrMap          as BA
import qualified Brick.Focus            as BF
import qualified Brick.Widgets.List     as BL
import qualified Brick.Main             as BM
import qualified Brick.Widgets.Edit     as BE
import qualified Brick.Types            as BT
import qualified Data.Vector            as Vec
import qualified Graphics.Vty           as V
import qualified TodoIO                 as IO
import qualified TodoItem               as TI

-- Types

data Name
    = List
    | Edit
    deriving (Eq, Ord, Show)

data EditMode
    = None
    | AddNew
    | EditEntry
    | SearchEntry
    deriving (Eq, Ord, Show)

data St = St
    { _file :: String
    , _focusRing :: BF.FocusRing Name
    , _list :: BL.List Name TI.TodoItem
    , _edit :: BE.Editor String Name
    , _editMode :: EditMode
    }
makeLenses ''St

-- Drawing

drawUI :: St -> [BT.Widget Name]
drawUI st = [ui]
    where
        f = st^.focusRing
        l = st^.list
        e = st^.edit
        label = str "hs-todo 1.0.0 - Your tasks (" <+> cur <+> str " of "
            <+> total <+> str ")"
        cur = case l^.BL.listSelectedL of
                Nothing -> str "-"
                Just i  -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.BL.listElementsL
        ui = vBox [ label
                  , BF.withFocusRing f (BL.renderList listDrawElement) l
                  , BF.withFocusRing f (BE.renderEditor (str. unlines)) e
                  ]

listDrawElement :: (Show a) => Bool -> a -> BT.Widget Name
listDrawElement _ l = str (show l)


-- Handling events

appHandleEvent :: St -> BT.BrickEvent Name e -> BT.EventM Name (BT.Next St)
appHandleEvent st (BT.VtyEvent e) =
    case BF.focusGetCurrent (st^.focusRing) of
      Just List -> case e of
            V.EvKey V.KEsc [] -> BM.halt st
            V.EvKey (V.KChar 'q') [] -> BM.halt st
            V.EvKey (V.KChar 'w') [] -> BM.continue =<< liftIO (handleWriteFile st)
            V.EvKey (V.KChar '/') [] -> BM.continue $ enterEdit st SearchEntry
            V.EvKey (V.KChar ' ') [] -> BM.continue $ st & list %~ BL.listModify TI.toggleDone
            V.EvKey (V.KChar 'n') [] -> BM.continue $ enterEdit st AddNew
            V.EvKey (V.KChar 'x') [] -> BM.continue $ deleteEntry st
            V.EvKey (V.KChar 'e') [] -> BM.continue . setEditContent "" $ enterEdit st EditEntry
            _ -> BM.continue =<< BT.handleEventLensed st list (BL.handleListEventVi BL.handleListEvent) e
      Just Edit -> case e of
            V.EvKey V.KEsc []   -> BM.continue . clearEdit $ leaveEdit st
            V.EvKey V.KEnter [] -> BM.continue . leaveEdit $ handleEditInput st
            _ -> BM.continue =<< BT.handleEventLensed st edit BE.handleEditorEvent e
      Nothing -> BM.continue st
appHandleEvent st _ = BM.continue st

handleWriteFile :: St -> IO St
handleWriteFile st = do
    let path = st^.file
        items = Vec.toList $ st ^. (list . BL.listElementsL)
    IO.writeTodoFile path items
    return st

enterEdit :: St -> EditMode -> St
enterEdit st m = clearEdit $ st & focusRing %~ BF.focusSetCurrent Edit
                                & editMode .~ m

setEditContent ::  String -> St -> St
setEditContent s st = st & edit .~ BE.editor Edit (Just 1) s

handleEditInput :: St -> St
handleEditInput st =
    case st^.editMode of
      AddNew    -> addNewEntry st
      _         -> st

addNewEntry :: St -> St
addNewEntry st =
    case parseValidInput $ BE.getEditContents (st^.edit) of
      Left err       -> setEditContent err st
      Right todoItem -> do
          let pos = getNextPostion st
              msg = "Successfully added new item"
          setEditContent msg $ st & list %~ BL.listInsert pos todoItem

getNextPostion :: St -> Int
getNextPostion st = Vec.length $ st ^. (list . BL.listElementsL)

parseValidInput :: [String] -> Either String TI.TodoItem
parseValidInput input =
    if null input || null (head input)
       then Left "Can't add new item. Input is empty!"
       else TI.parseTodoItem $ head input

deleteEntry :: St -> St
deleteEntry st =
    case st ^. (list . BL.listSelectedL) of
      Nothing -> st
      Just i  -> do
          let msg = "Deleted selected item"
          setEditContent msg $ st & list %~ BL.listRemove i

clearEdit :: St -> St
clearEdit st = st & edit %~ BE.applyEdit clearZipper

leaveEdit :: St -> St
leaveEdit st = st & focusRing %~ BF.focusSetCurrent List

-- Attributes

appAttrMap :: BA.AttrMap
appAttrMap = BA.attrMap V.defAttr
    [ (BL.listAttr,            V.white `on` V.black)
    , (BL.listSelectedAttr,    V.black `on` V.yellow)
    ]

-- App definition

todoApp :: BM.App St e Name
todoApp = BM.App { BM.appDraw = drawUI
                 , BM.appChooseCursor = BM.showFirstCursor
                 , BM.appHandleEvent = appHandleEvent
                 , BM.appStartEvent = return
                 , BM.appAttrMap = const appAttrMap
                 }

initialState :: String -> [TI.TodoItem] -> St
initialState path todoItems =
    St path
       (BF.focusRing [List, Edit])
       (BL.list List (Vec.fromList todoItems) 1)
       (BE.editor Edit (Just 1) "")
       None

-- Start brick app

runMain :: String -> [TI.TodoItem] -> IO [TI.TodoItem]
runMain path todoItems = do
    st <- BM.defaultMain todoApp (initialState path todoItems)
    return (Vec.toList $ st ^. (list . BL.listElementsL))
