module Main where

import Brick.Util (fg, on)
import Brick.Widgets.Core (str, withAttr)
import Control.Monad (void)

import qualified Brick.AttrMap          as BA
import qualified Brick.Widgets.Center   as BC
import qualified Brick.Widgets.List     as BL
import qualified Brick.Main             as BM
import qualified Brick.Types            as BT
import qualified Data.Vector            as Vec
import qualified Graphics.Vty           as V

drawUI :: (Show a) => BL.List () a -> [BT.Widget ()]
drawUI l = [BL.renderList listDrawElement True l]

listDrawElement :: (Show a) => Bool -> a -> BT.Widget ()
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str s)
                   else str s
    in BC.hCenter $ selStr (show a)

appHandleEvent :: BL.List () String -> BT.BrickEvent () e
               -> BT.EventM () (BT.Next (BL.List () String))
appHandleEvent l (BT.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> BM.halt l
        V.EvKey (V.KChar 'q') [] -> BM.halt l
        x -> BM.continue =<< BL.handleListEventVi BL.handleListEvent x l
appHandleEvent l _ = BM.continue l

customAttr :: BA.AttrName
customAttr = BA.attrName "customAttr"

appAttrMap :: BA.AttrMap
appAttrMap = BA.attrMap V.defAttr
    [ (BL.listAttr,            V.white `on` V.black)
    , (BL.listSelectedAttr,    V.black `on` V.white)
    , (customAttr,            fg V.black)
    ]

todoApp :: BM.App (BL.List () String) e ()
todoApp = BM.App { BM.appDraw = drawUI
                 , BM.appChooseCursor = BM.showFirstCursor
                 , BM.appHandleEvent = appHandleEvent
                 , BM.appStartEvent = return
                 , BM.appAttrMap = const appAttrMap
                 }

initialState :: BL.List () String
initialState = BL.list () (Vec.fromList ["hello","world","!"]) 1

main :: IO ()
main = void $ BM.defaultMain todoApp initialState
