{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (Right, Left)

import qualified Graphics.Vty as V
import qualified Brick.Main as M

import Data.Default
import Control.Concurrent (newChan, writeChan, threadDelay, forkIO)
import Control.Monad (void, forever)
import qualified System.Random as R

import qualified Types
import qualified State
import qualified View

main :: IO ()
main = do
  let app = M.App { M.appDraw = View.drawUi
                  , M.appStartEvent = return
                  , M.appHandleEvent = State.appHandleEvent
                  , M.appAttrMap = const View.snakeAttrMap
                  , M.appLiftVtyEvent = Types.VtyEvent
                  , M.appChooseCursor = M.neverShowCursor
                  }

  stdGen <- R.getStdGen
  let locs =  R.randomRs (1, 9) stdGen
  let initialState = State.makeInitialState locs

  chan <- newChan

  void $ forkIO $ forever $ do
    threadDelay 250000
    writeChan chan Types.Tick

  void $ M.customMain (V.mkVty def) chan app initialState
