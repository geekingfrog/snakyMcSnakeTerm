{-# LANGUAGE OverloadedStrings #-}

module State (
    makeInitialState
  , resetState
  , appHandleEvent
  , hasCollision
  , collideWithSnake
  , collideWithWall
  ) where


import Data.Maybe (fromMaybe)
import Lens.Micro ((^.), (&), (%~), (.~))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable

import qualified Brick.Main as Brick
import Brick.Types (Location(..))
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Types as BT
import qualified Graphics.Vty as V

import Types as T
import Constants (boardW, boardH)


-- orphan instance, get git version of brick where this is fixed
instance Eq Location where
  (Location (x1, y1)) == (Location (x2, y2)) = x1 == x2 && y1 == y2


makeInitialState :: [Int] -> St
makeInitialState locs =
  let
    (x:y:locs') = locs
    choices = [ ("Restart", Restart) , ("Quit", Quit) ]
    deathD = D.dialog (Just "Game over") (Just (0, choices)) 50
    s0 = St
      { _snake = Seq.fromList [Location (1, 1), Location (1, 2), Location (1, 3)]
      , _direction = T.Right
      , _nextDirection = T.Right
      , _fruit = Location (x, y)
      , _fruitLocations = locs'
      , _deathDialog = deathD
      }
    (fruitPos, locs'') = newFruitPosition s0
  in
    s0 {_fruitLocations = locs'', _fruit = fruitPos}


resetState :: St -> St
resetState st = makeInitialState (st ^. fruitLocations)

appHandleEvent :: St -> T.SnakeEvent -> BT.EventM T.WidgetName (BT.Next St)
appHandleEvent = withDialogEvent appHandleEvent'

appHandleEvent' :: St -> SnakeEvent -> BT.EventM WidgetName (BT.Next St)
appHandleEvent' st (VtyEvent (V.EvKey V.KUp [])) = Brick.continue $ st & nextDirection %~ changeDir Up (st ^. direction)
appHandleEvent' st (VtyEvent (V.EvKey V.KDown [])) = Brick.continue $ st & nextDirection %~ changeDir Down (st ^. direction)
appHandleEvent' st (VtyEvent (V.EvKey V.KLeft [])) = Brick.continue $ st & nextDirection %~ changeDir T.Left (st ^. direction)
appHandleEvent' st (VtyEvent (V.EvKey V.KRight [])) = Brick.continue $ st & nextDirection %~ changeDir T.Right (st ^. direction)
appHandleEvent' st (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = Brick.halt st
appHandleEvent' st (VtyEvent (V.EvKey (V.KChar 'q') [])) = Brick.halt st
appHandleEvent' st (VtyEvent (V.EvKey V.KEsc [])) = Brick.halt st
appHandleEvent' st (VtyEvent (V.EvKey V.KEnter [])) =
  case D.dialogSelection (st ^. deathDialog) of
    Nothing -> Brick.continue st
    Just Restart -> Brick.continue (resetState st)
    Just Quit -> Brick.halt st

appHandleEvent' st Tick =
  if hasCollision st
    then Brick.continue st
    else Brick.continue (updateState st)

appHandleEvent' st _ = Brick.continue st

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


changeDir :: Direction -> Direction -> Direction -> Direction
changeDir newDir curDir nextDir =
  let
    changeDir' Up Down = Nothing
    changeDir' Down Up = Nothing
    changeDir' T.Left T.Right = Nothing
    changeDir' T.Right T.Left = Nothing
    changeDir' new _ = Just new
  in
    fromMaybe nextDir (changeDir' newDir curDir)



translateDir :: Location -> Direction -> Location
translateDir (Location (x,y)) Up = Location (x, y-1)
translateDir (Location (x,y)) Down = Location (x, y+1)
translateDir (Location (x,y)) T.Left = Location (x-1, y)
translateDir (Location (x,y)) T.Right = Location (x+1, y)


newFruitPosition :: St -> (Location, [Int])
newFruitPosition st =
  let
    newX : newY : locs = st ^. fruitLocations
    newLoc = Location (newX, newY)
  in
    if collideWithSnake newLoc (st ^. snake)
       then newFruitPosition $ st {_fruitLocations = newY:locs}
       else (newLoc, locs)


collideWithWall :: St -> Bool
collideWithWall st =
  let
    (Location (x, y) Seq.:< _) = Seq.viewl $ st ^. snake
  in
    x < 0 || x > boardW || y <= 0 || y > boardH

collideWithSnake :: Location -> Snake -> Bool
collideWithSnake l = Foldable.any (== l)

hasCollision :: St -> Bool
hasCollision st =
  let
    (snHead Seq.:< snTail) = Seq.viewl (st ^. snake)
  in
    collideWithWall st || collideWithSnake snHead snTail


withDialogEvent
  :: (St -> SnakeEvent -> BT.EventM WidgetName (BT.Next St))
  -> St
  -> SnakeEvent
  -> BT.EventM WidgetName (BT.Next St)
withDialogEvent handle st ev =
  case ev of
    Tick -> handle st ev
    VtyEvent vtyEv -> do
      let d = st ^. deathDialog
      newDialog <- D.handleDialogEvent vtyEv d
      handle (st & deathDialog .~ newDialog) ev
