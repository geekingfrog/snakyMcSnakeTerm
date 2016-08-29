{-# LANGUAGE OverloadedStrings #-}

module View (
    snakeAttrMap
  , drawUi
) where

import Lens.Micro ((^.))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable

import Brick.AttrMap (AttrMap, attrMap)
import qualified Graphics.Vty as V
import Brick.Util (on, bg)
import qualified Brick.Widgets.Dialog as D
import Brick.Widgets.Core ((<+>))
import qualified Brick.Widgets.Core as C
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Border as Border
import qualified Brick.Types as BT
import Brick.Widgets.Center (centerLayer, hCenter)

import Constants (boardW, boardH)
import Types
import State (hasCollision, collideWithWall, collideWithSnake)

snakeAttrMap :: AttrMap
snakeAttrMap = attrMap V.defAttr
      [ ("crashed", V.red `on` V.black)
      , (D.dialogAttr, V.white `on` V.blue)
      , (D.buttonAttr, V.black `on` V.white)
      , (D.buttonSelectedAttr, bg V.yellow)
      ]


drawUi :: St -> [BT.Widget WidgetName]
drawUi st =
  let
    board = centerLayer $ C.withBorderStyle BS.unicodeBold $
      Border.border $ C.hLimit boardW $ C.vLimit boardH $ C.fill ' '
    dialogBody = C.vBox [C.padAll 1 $ hCenter $ C.str "(<tab> / <S-tab> to select)"]
    d = [D.renderDialog (st ^. deathDialog) dialogBody | hasCollision st]
  in
    d ++ drawSnake st ++ [drawFruit (st ^. fruit), board, debug st]


drawSnake :: St -> [BT.Widget WidgetName]
drawSnake st =
  let
    (snHead Seq.:< _) = Seq.viewl $ st ^. snake
    headCursor = centerInBoard snHead (C.str "o")
    tailCursors = fmap (\l -> centerInBoard l $ C.str "#") (st ^. snake)
    headAttr = if hasCollision st then "crashed" else ""
  in
    C.withAttr headAttr headCursor : Foldable.toList tailCursors

drawFruit :: BT.Location -> BT.Widget WidgetName
drawFruit loc = centerInBoard loc $ C.str "@"

centerInBoard :: BT.Location -> BT.Widget n -> BT.Widget n
centerInBoard (BT.Location (x, y)) = C.translateBy (BT.Location (x - boardW `div` 2, y - boardH `div` 2)) . centerLayer


debug :: St -> BT.Widget WidgetName
debug st =
  let
    fruitPos = C.padBottom BT.Max $ C.str $ show (take 10 $ st ^. fruitLocations)
    wallCollided = collideWithWall st
    (snHead Seq.:< snTail) = Seq.viewl (st ^. snake)
    snakeCollision = collideWithSnake snHead snTail
  in
    fruitPos <+> C.str (show wallCollided) <+> C.str (show snakeCollision)
