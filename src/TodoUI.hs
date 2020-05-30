{-# LANGUAGE TemplateHaskell #-}

module TodoUI
( runMain
) where

import Brick.Util (on)
import Brick.Widgets.Core (str, vBox, (<+>))
import Lens.Micro
import Lens.Micro.TH

import qualified Brick.AttrMap          as BA
import qualified Brick.Widgets.List     as BL
import qualified Brick.Main             as BM
import qualified Brick.Types            as BT
import qualified Data.Vector            as Vec
import qualified Graphics.Vty           as V
import qualified TodoItem               as TI

data St =
    St { _file :: String
       , _itemFilter :: String
       , _list :: BL.List () TI.TodoItem
       }
makeLenses ''St

drawUI :: St -> [BT.Widget ()]
drawUI st = [ui]
    where
        l = st^.list
        -- filteredList = listFilterElements l (st^.itemFilter)
        label = str "hs-todo 1.0.0 - Your tasks (" <+> cur <+> str " of "
            <+> total <+> str ")"
        cur = case l^.BL.listSelectedL of
                Nothing -> str "-"
                Just i  -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.BL.listElementsL
        ui = vBox [ label
                  , BL.renderList listDrawElement True l
                  ]

-- listFilterElements :: BL.List () TI.TodoItem -> String -> BL.List() TI.TodoItem
-- listFilterElements l _ = do
--     let ti = Vec.toList $ l^.BL.listElementsL
--         x = ti
--     BL.list () (Vec.fromList x) 1

listDrawElement :: (Show a) => Bool -> a -> BT.Widget ()
listDrawElement _ l = str (show l)

setFilter :: String -> String
setFilter _ = "phone"

appHandleEvent :: St -> BT.BrickEvent () e -> BT.EventM () (BT.Next St)
appHandleEvent st (BT.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> BM.halt st
        V.EvKey (V.KChar 'q') [] -> BM.halt st
        V.EvKey (V.KChar ' ') [] -> BM.continue $ st & list %~ BL.listModify TI.toggleDone
        V.EvKey (V.KChar 'w') [] -> BM.continue st
        V.EvKey (V.KChar 'f') [] -> BM.continue $ st & itemFilter %~ setFilter
        _ -> BM.continue =<< BT.handleEventLensed st list (BL.handleListEventVi BL.handleListEvent) e
appHandleEvent st _ = BM.continue st

appAttrMap :: BA.AttrMap
appAttrMap = BA.attrMap V.defAttr
    [ (BL.listAttr,            V.white `on` V.black)
    , (BL.listSelectedAttr,    V.black `on` V.yellow)
    ]

todoApp :: BM.App St e ()
todoApp = BM.App { BM.appDraw = drawUI
                 , BM.appChooseCursor = BM.showFirstCursor
                 , BM.appHandleEvent = appHandleEvent
                 , BM.appStartEvent = return
                 , BM.appAttrMap = const appAttrMap
                 }

initialState :: String -> [TI.TodoItem] -> St
initialState path todoItems =
    St (path)
       ("")
       (BL.list () (Vec.fromList todoItems) 1)

runMain :: String -> [TI.TodoItem] -> IO [TI.TodoItem]
runMain path todoItems = do
    st <- BM.defaultMain todoApp (initialState path todoItems)
    return (Vec.toList $ st ^. (list . BL.listElementsL))
