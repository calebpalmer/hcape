{-# LANGUAGE BangPatterns #-}
module Hcape.GameLogic
       where

import Hcape.Types

import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Rect

import Control.Monad.State
import Data.Time.Clock

--background type
data Background = Background {
  bgImageFile :: String,
  bgSurface :: Maybe Surface,
  bgVisible :: Bool
  }
                  
instance Drawable Background where
  drawOrder _ = DoBackground
  surface bg = bgSurface bg
  loadSurface bg newSurface = bg {bgSurface = Just newSurface}
  filePath bg = bgImageFile bg
  coordinates _ = Just $ Rect 0 0 0 0
  isVisible bg = bgVisible bg
  tag bg = "background"
  processEvents bg _ _ _ _ = bg
  
-- default configurations
defaultConfig :: ScreenConfig
defaultConfig = ScreenConfig 1280 800 0 Nothing