module TodoUI
( runMain
) where

import Brick.Util (on)
import Brick.Widgets.Core (str, vBox, (<+>))
import Control.Monad (void)
import Lens.Micro ((^.))

import qualified Brick.AttrMap          as BA
import qualified Brick.Widgets.List     as BL
import qualified Brick.Main             as BM
import qualified Brick.Types            as BT
import qualified Data.Vector            as Vec
import qualified Graphics.Vty           as V
import qualified TodoItem               as TI

drawUI :: (Show a) => BL.List () a -> [BT.Widget ()]
drawUI l = [ui]
    where
        label = str "hs-todo 1.0.0 - Your tasks (" <+> cur <+> str " of "
            <+> total <+> str ")"
        cur = case l^.BL.listSelectedL of
                Nothing -> str "-"
                Just i  -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.BL.listElementsL
        list = BL.renderList listDrawElement True l
        ui = vBox [ label
                  , list
                  ]

listDrawElement :: (Show a) => Bool -> a -> BT.Widget ()
listDrawElement _ a = str (show a)

appHandleEvent :: BL.List () TI.TodoItem -> BT.BrickEvent () e
               -> BT.EventM () (BT.Next (BL.List () TI.TodoItem))
appHandleEvent l (BT.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> BM.halt l
        V.EvKey (V.KChar 'q') [] -> BM.halt l
        x -> BM.continue =<< BL.handleListEventVi BL.handleListEvent x l
appHandleEvent l _ = BM.continue l

appAttrMap :: BA.AttrMap
appAttrMap = BA.attrMap V.defAttr
    [ (BL.listAttr,            V.white `on` V.black)
    , (BL.listSelectedAttr,    V.black `on` V.yellow)
    ]

todoApp :: BM.App (BL.List () TI.TodoItem) e ()
todoApp = BM.App { BM.appDraw = drawUI
                 , BM.appChooseCursor = BM.showFirstCursor
                 , BM.appHandleEvent = appHandleEvent
                 , BM.appStartEvent = return
                 , BM.appAttrMap = const appAttrMap
                 }

initialState :: [TI.TodoItem] -> BL.List () TI.TodoItem
initialState todoItems = BL.list () (Vec.fromList todoItems) 1

runMain :: [TI.TodoItem] -> IO()
runMain todoItems = void $ BM.defaultMain todoApp (initialState todoItems)
