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

data Direction = Up | Down | Left | Right deriving (Show, Eq)
type Snake = Seq.Seq T.Location

data St = St
  { _snake :: Snake
  , _direction :: Direction
  , _timer :: Int
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
  | Counter
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
  let (x:y:locs) =  R.randomRs (0, 10) stdGen
  let initialState = St {
      _snake = Seq.fromList [T.Location (1, 1), T.Location (1, 2), T.Location (1, 3)]
    , _direction = Right
    , _timer = 0
    , _fruit = T.Location (x, y)
    , _fruitLocations = locs
    }

  chan <- newChan

  void $ forkIO $ forever $ do
    threadDelay 500000
    writeChan chan Counter

  void $ M.customMain (V.mkVty def) chan app initialState


drawUi :: St -> [T.Widget WidgetName]
drawUi st = drawCounter st : drawSnake st ++ [drawFruit (st ^. fruit), board, debug st]
  where
    board = centerLayer $ Border.border $ C.hLimit 40 $ C.vLimit 20 $ C.fill ' '

drawSnake :: St -> [T.Widget WidgetName]
drawSnake st = toList $ fmap (\(T.Location (x, y)) -> C.translateBy (T.Location (x-20, y-10)) $ centerLayer $ C.str "#") (st^.snake)
-- drawSnake st = toList $ fmap (\loc -> C.translateBy (T.Location (-20, -10)) $ centerLayer $ C.translateBy loc $ C.str "#") (st^.snake)

drawCounter :: St -> T.Widget WidgetName
drawCounter st =
    C.translateBy (T.Location (0, -12))
  $ centerLayer
  $ C.str (show $ st ^. timer) <+> C.str (" " ++ show (st ^. direction))

drawFruit :: T.Location -> T.Widget WidgetName
drawFruit (T.Location (x, y)) = C.translateBy (T.Location (x-20, y-10)) $ centerLayer $ C.str "O"

debug :: St -> T.Widget WidgetName
debug st = C.padBottom T.Max $ C.str $ show (take 10 $ st ^. fruitLocations)

appEvent :: St -> SnakeEvent -> T.EventM WidgetName (T.Next St)
appEvent st Counter = M.continue $ moveSnake $ st & timer %~ (+1)
appEvent st (VtyEvent (V.EvKey V.KUp [])) = M.continue $ st & direction .~ Up
appEvent st (VtyEvent (V.EvKey V.KDown [])) = M.continue $ st & direction .~ Down
appEvent st (VtyEvent (V.EvKey V.KLeft [])) = M.continue $ st & direction .~ Left
appEvent st (VtyEvent (V.EvKey V.KRight [])) = M.continue $ st & direction .~ Right
appEvent st (VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st _ = M.continue st

instance Eq T.Location where
  (T.Location (x1, y1)) == (T.Location (x2, y2)) = x1 == x2 && y1 == y2

moveSnake :: St -> St
moveSnake st =
  let
    -- snake is always at least 3 segment long
    (snTail Seq.:> _) = Seq.viewr $ st ^. snake
    (snHead Seq.:< _) = Seq.viewl $ st ^. snake
    newHead = translateDir snHead (st ^. direction)
    newSnake = (Seq.<|) newHead (if st ^. fruit == snHead then st ^. snake else snTail)
  in
    st {_snake = newSnake}

translateDir :: T.Location -> Direction -> T.Location
translateDir (T.Location (x, y)) dir
  | dir == Up = T.Location (x, y-1)
  | dir == Down = T.Location (x, y+1)
  | dir == Left = T.Location (x-1, y)
  | dir == Right = T.Location (x+1, y)
