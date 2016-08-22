{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Border.Style as Border

import Brick.Widgets.Center
  ( center
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

data Direction = Up | Down | Left | Right deriving (Show, Eq)
type Snake = Seq.Seq T.Location

data St = St
  { _snake :: Snake
  , _direction :: Direction
  , _nextDirection :: Direction
  , _fruit :: T.Location
  , _fruitLocations :: [Int]
  }
  deriving Show

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
  let app = M.App { M.appDraw = drawUi
                  , M.appStartEvent = return
                  , M.appHandleEvent = appEvent
                  , M.appAttrMap = const def
                  , M.appLiftVtyEvent = VtyEvent
                  , M.appChooseCursor = M.neverShowCursor
                  }

  stdGen <- R.getStdGen
  let (x:y:locs) =  R.randomRs (1, 9) stdGen
  let initialState = St {
      _snake = Seq.fromList [T.Location (1, 1), T.Location (1, 2), T.Location (1, 3)]
    , _direction = Right
    , _nextDirection = Right
    , _fruit = T.Location (x, y)
    , _fruitLocations = locs
    }

  chan <- newChan

  void $ forkIO $ forever $ do
    threadDelay 250000
    writeChan chan Tick

  void $ M.customMain (V.mkVty def) chan app initialState


drawUi :: St -> [T.Widget WidgetName]
drawUi st = drawSnake st ++ [drawFruit (st ^. fruit), board, debug st]
  where
    board = centerLayer $ Border.border $ C.hLimit 40 $ C.vLimit 20 $ C.fill ' '

drawSnake :: St -> [T.Widget WidgetName]
drawSnake st = toList $ fmap (\(T.Location (x, y)) -> C.translateBy (T.Location (x-20, y-10)) $ centerLayer $ C.str "#") (st^.snake)
-- drawSnake st = toList $ fmap (\loc -> C.translateBy (T.Location (-20, -10)) $ centerLayer $ C.translateBy loc $ C.str "#") (st^.snake)

drawFruit :: T.Location -> T.Widget WidgetName
drawFruit (T.Location (x, y)) = C.translateBy (T.Location (x-20, y-10)) $ centerLayer $ C.str "O"

debug :: St -> T.Widget WidgetName
debug st = C.padBottom T.Max $ C.str $ show (take 10 $ st ^. fruitLocations)

appEvent :: St -> SnakeEvent -> T.EventM WidgetName (T.Next St)
appEvent st (VtyEvent (V.EvKey V.KUp [])) = M.continue $ st & nextDirection %~ changeDir Up (st ^. direction)
appEvent st (VtyEvent (V.EvKey V.KDown [])) = M.continue $ st & nextDirection %~ changeDir Down (st ^. direction)
appEvent st (VtyEvent (V.EvKey V.KLeft [])) = M.continue $ st & nextDirection %~ changeDir Left (st ^. direction)
appEvent st (VtyEvent (V.EvKey V.KRight [])) = M.continue $ st & nextDirection %~ changeDir Right (st ^. direction)
appEvent st (VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st Tick =
  let
    newState = st & direction .~ (st ^. nextDirection)
  in
    M.continue $ moveSnake newState

appEvent st _ = M.continue st

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

moveSnake :: St -> St
moveSnake st =
  let
    -- snake is always at least 3 segment long
    (snTail Seq.:> _) = Seq.viewr $ st ^. snake
    (snHead Seq.:< _) = Seq.viewl $ st ^. snake
    newHead = translateDir snHead (st ^. direction)
    ateFruit = st ^. fruit == snHead
    newSnake = (Seq.<|) newHead (if ateFruit then st ^. snake else snTail)
    (newFruit, futureLocs) = if ateFruit then newFruitPosition st else (st ^. fruit, st ^. fruitLocations)
  in
    st {_snake = newSnake, _fruit = newFruit, _fruitLocations = futureLocs}

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
    hasCollision = F.any (== newLoc) (st ^. snake)
  in
    if hasCollision
       then newFruitPosition $ st {_fruitLocations = newY:locs}
       else (newLoc, locs)
