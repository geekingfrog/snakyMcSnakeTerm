{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (Right, Left)
import Lens.Micro ((^.), (&), (%~), (.~))
import Lens.Micro.TH (makeLenses)

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Widgets.Core as C
import Brick.Widgets.Core
  ( (<=>)
  , (<+>)
  )

import Brick.Widgets.Border
  ( vBorder
  , border
  )

import qualified Brick.Widgets.Dialog as D

import Brick.Util (on, fg, bg)
import Brick.AttrMap (attrMap, AttrMap)

import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Border.Style as Border

import Brick.Widgets.Center
  ( center
  , hCenter
  , centerLayer
  )

import qualified Brick.Types as T
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.Default
import Control.Concurrent (newChan, writeChan, threadDelay, forkIO)
import Control.Monad (void, forever)
import qualified System.Random as R
import qualified Data.Foldable as F
import Data.Maybe (fromMaybe)


-- TODO put that in the state or a reader env
boardW :: Int
boardW = 20 :: Int
boardH :: Int
boardH = 10 :: Int


data Direction = Up | Down | Left | Right deriving (Show, Eq)
data Choice = Restart | Quit deriving (Show)
type Snake = Seq.Seq T.Location

data St = St
  { _snake :: Snake
  , _direction :: Direction
  , _nextDirection :: Direction
  , _fruit :: T.Location
  , _fruitLocations :: ![Int]
  , _deathDialog :: D.Dialog Choice
  }

makeLenses ''St

data WidgetName =
    BoardW
  | SnakeW
  | ViewportBoard
  deriving (Show, Eq)

data SnakeEvent =
    VtyEvent V.Event
  | Tick
  deriving (Show, Eq)

main :: IO ()
main = do
  let globalDefault = V.defAttr
  let theMap = attrMap globalDefault [
          ("crashed", V.red `on` V.black)
        , (D.dialogAttr, V.white `on` V.blue)
        , (D.buttonAttr, V.black `on` V.white)
        , (D.buttonSelectedAttr, bg V.yellow)
        ]

  let app = M.App { M.appDraw = drawUi
                  , M.appStartEvent = return
                  , M.appHandleEvent = withDialogEvent appEvent
                  , M.appAttrMap = const theMap
                  , M.appLiftVtyEvent = VtyEvent
                  , M.appChooseCursor = M.neverShowCursor
                  }

  stdGen <- R.getStdGen
  let locs =  R.randomRs (1, 9) stdGen
  let choices = [ ("Restart", Restart) , ("Quit", Quit) ]
  let deathD = D.dialog (Just "Game over") (Just (0, choices)) 50
  let initialState = makeInitialState locs

  chan <- newChan

  void $ forkIO $ forever $ do
    threadDelay 250000
    writeChan chan Tick

  void $ M.customMain (V.mkVty def) chan app initialState


makeInitialState :: [Int] -> St
makeInitialState locs =
  let
    (x:y:locs') = locs
    choices = [ ("Restart", Restart) , ("Quit", Quit) ]
    deathD = D.dialog (Just "Game over") (Just (0, choices)) 50
    s0 = St
      { _snake = Seq.fromList [T.Location (1, 1), T.Location (1, 2), T.Location (1, 3)]
      , _direction = Right
      , _nextDirection = Right
      , _fruit = T.Location (x, y)
      , _fruitLocations = locs'
      , _deathDialog = deathD
      }
    (fruitPos, locs'') = newFruitPosition s0
  in
    s0 {_fruitLocations = locs'', _fruit = fruitPos}

resetState :: St -> St
resetState st = makeInitialState (st ^. fruitLocations)

drawUi :: St -> [T.Widget WidgetName]
drawUi st =
  let
    board = centerLayer $ Border.border $ C.hLimit boardW $ C.vLimit boardH $ C.fill ' '
    d = if hasCollision st then [D.renderDialog (st ^. deathDialog) C.emptyWidget] else []
  in
    d ++ drawSnake st ++ [drawFruit (st ^. fruit), board, debug st]

drawSnake :: St -> [T.Widget WidgetName]
drawSnake st =
  let
    (snHead Seq.:< snTail) = Seq.viewl $ st ^. snake
    headCursor = centerInBoard snHead (C.str "o")
    tailCursors = fmap (\l -> centerInBoard l $ C.str "#") (st ^. snake)
    headAttr = if hasCollision st then "crashed" else ""
  in
    C.withAttr headAttr headCursor : toList tailCursors

drawFruit :: T.Location -> T.Widget WidgetName
drawFruit loc = centerInBoard loc $ C.str "@"

centerInBoard :: T.Location -> Widget n -> Widget n
centerInBoard (T.Location (x, y)) = C.translateBy (T.Location (x - boardW `div` 2, y - boardH `div` 2)) . centerLayer

debug :: St -> T.Widget WidgetName
debug st =
  let
    fruitPos = C.padBottom T.Max $ C.str $ show (take 10 $ st ^. fruitLocations)
    wallCollided = collideWithWall st
    (snHead Seq.:< snTail) = Seq.viewl (st ^. snake)
    snakeCollision = collideWithSnake snHead snTail
  in
    fruitPos <+> (C.str $ show wallCollided) <+> (C.str $ show snakeCollision)
    -- [fruitPos, wallCollided]

appEvent :: St -> SnakeEvent -> T.EventM WidgetName (T.Next St)
appEvent st (VtyEvent (V.EvKey V.KUp [])) = M.continue $ st & nextDirection %~ changeDir Up (st ^. direction)
appEvent st (VtyEvent (V.EvKey V.KDown [])) = M.continue $ st & nextDirection %~ changeDir Down (st ^. direction)
appEvent st (VtyEvent (V.EvKey V.KLeft [])) = M.continue $ st & nextDirection %~ changeDir Left (st ^. direction)
appEvent st (VtyEvent (V.EvKey V.KRight [])) = M.continue $ st & nextDirection %~ changeDir Right (st ^. direction)
appEvent st (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt st
appEvent st (VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt st
appEvent st (VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st (VtyEvent (V.EvKey V.KEnter [])) =
  case D.dialogSelection (st ^. deathDialog) of
    Nothing -> M.continue st
    Just Restart -> M.continue (resetState st)
    Just Quit -> M.halt st

appEvent st Tick =
  if hasCollision st
    then M.continue st
    else M.continue (updateState st)

appEvent st ev = M.continue st

instance Eq T.Location where
  (T.Location (x1, y1)) == (T.Location (x2, y2)) = x1 == x2 && y1 == y2

changeDir :: Direction -> Direction -> Direction -> Direction
changeDir newDir curDir nextDir =
  let
    changeDir' Up Down = Nothing
    changeDir' Down Up = Nothing
    changeDir' Left Right = Nothing
    changeDir' Right Left = Nothing
    changeDir' new _ = Just new
  in
    fromMaybe nextDir (changeDir' newDir curDir)

updateState :: St -> St
updateState st =
  let
    -- snake is always at least 3 segment long
    (snTail Seq.:> _) = Seq.viewr $ st ^. snake
    (snHead Seq.:< _) = Seq.viewl $ st ^. snake
    newHead = translateDir snHead (st ^. nextDirection)
    ateFruit = st ^. fruit == newHead
    newSnake = (Seq.<|) newHead (if ateFruit then st ^. snake else snTail)
    (newFruit, futureLocs) = if ateFruit then newFruitPosition st else (st ^. fruit, st ^. fruitLocations)
  in
    st
      { _snake = newSnake
      , _fruit = newFruit
      , _fruitLocations = futureLocs
      , _direction = st ^. nextDirection
      }

translateDir :: T.Location -> Direction -> T.Location
translateDir (T.Location (x, y)) dir
  | dir == Up = T.Location (x, y-1)
  | dir == Down = T.Location (x, y+1)
  | dir == Left = T.Location (x-1, y)
  | dir == Right = T.Location (x+1, y)

newFruitPosition :: St -> (T.Location, [Int])
newFruitPosition st =
  let
    newX : newY : locs = st ^. fruitLocations
    newLoc = T.Location (newX, newY)
  in
    if collideWithSnake newLoc (st ^. snake)
       then newFruitPosition $ st {_fruitLocations = newY:locs}
       else (newLoc, locs)

collideWithWall :: St -> Bool
collideWithWall st =
  let
    ((T.Location (x, y)) Seq.:< _) = Seq.viewl $ st ^. snake
  in
    x < 0 || x > boardW || y <= 0 || y > boardH

collideWithSnake :: T.Location -> Snake -> Bool
collideWithSnake loc = F.any (== loc)

hasCollision :: St -> Bool
hasCollision st =
  let
    (snHead Seq.:< snTail) = Seq.viewl (st ^. snake)
  in
    collideWithWall st || collideWithSnake snHead snTail


withDialogEvent
  :: (St -> SnakeEvent -> T.EventM WidgetName (T.Next St))
  -> St
  -> SnakeEvent
  -> T.EventM WidgetName (T.Next St)
withDialogEvent handle st ev = do
  case ev of
    Tick -> handle st ev
    VtyEvent vtyEv -> do
      let d = st ^. deathDialog
      newDialog <- D.handleDialogEvent vtyEv d
      handle (st & deathDialog .~ newDialog) ev
