{-# LANGUAGE TemplateHaskell #-}

module Types where

import Lens.Micro.TH (makeLenses)
import qualified Data.Sequence as Seq
import qualified Brick.Types as T
import Brick.Widgets.Dialog (Dialog)
import qualified Graphics.Vty as V

data Direction = Up | Down | Left | Right deriving (Show, Eq)
data Choice = Restart | Quit deriving (Show)
type Snake = Seq.Seq T.Location

data St = St
  { _snake :: Snake
  , _direction :: Direction
  , _nextDirection :: Direction
  , _fruit :: T.Location
  , _fruitLocations :: ![Int]
  , _deathDialog :: Dialog Choice
  }

makeLenses ''St

data WidgetName =
    BoardW
  | SnakeW
  | ViewportBoard
  deriving (Show, Eq, Ord)

data SnakeEvent =
    VtyEvent V.Event
  | Tick
  deriving (Show, Eq)
