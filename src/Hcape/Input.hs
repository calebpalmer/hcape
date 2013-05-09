{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hcape.Input 
       (
         handleEvent
       )
       
       where

import qualified Graphics.UI.SDL.General as S
import Graphics.UI.SDL.Events
import Control.Monad.Trans
import System.Exit
import Control.Monad.State
import Data.Time.Clock

import Hcape.Types

-- Recursive
handleEvent :: MyGame ()
handleEvent = do
  event <- liftIO pollEvent
  if event == NoEvent then return ()
    else do
    checkEvent event
    gameState <- get
    let lastUpdateTime = lastGameUpdate gameState
        tStep = 1 / timeStep gameState
    currentTime <- liftIO $ getCurrentTime
    case lastUpdateTime of Nothing -> do 
                             put gameState {lastGameUpdate = Just currentTime}
                             handleEvent
                           Just time -> do
                             let timeDiff = diffUTCTime currentTime time
                             if (timeDiff) >= (fromRational $ toRational tStep :: NominalDiffTime) then do
                               return ()
                               else do
                               handleEvent

  
checkEvent :: Event -> MyGame ()
checkEvent event = do
  case event of Quit -> exit
                NoEvent -> return ()
                realEvent -> do
                  gameState <- get
                  let events = eventQueue gameState
                      newEvents = (SDLGameEvent realEvent) : events
                  put gameState {eventQueue = newEvents}
                
exit :: MyGame ()
exit =  do 
  liftIO S.quit
  liftIO $ exitWith ExitSuccess