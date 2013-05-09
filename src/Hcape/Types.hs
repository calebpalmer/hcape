{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hcape.Types 
       (
         ScreenConfig(..)
       ,GameState(..)
       ,MyGame(..)
       ,DrawOrder(..)
       ,GameEvent(..)
       ,Drawable(..)
       ,AnyDrawable(..)
       ,Paddle(..)
       ,Ball(..)
       ,runMyGame
       )
       
       where

import Hcape.Geometry.Vector (Vector(..))
import Hcape.Geometry.Shapes (Shape(..))

import Control.Monad.Reader
import Control.Monad.State
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Rect
import Graphics.UI.SDL.Events
import Data.Time.Clock


data ScreenConfig = ScreenConfig { 
  xWinSize :: Int,
  yWinSize :: Int,
  pixelBits :: Int,
  mainSurface :: Maybe Surface
  } deriving Show
             
data GameState = GameState { 
  screenConfig :: ScreenConfig,
  eventQueue :: [GameEvent],
  lastGameUpdate :: Maybe UTCTime,
  timeStep :: Double,
  sBall :: Ball,
  sLeftPaddle :: Paddle,
  sRightPaddle :: Paddle,
  sDrawablesList :: [AnyDrawable]
  
  }

-- Game types
newtype MyGame a  = Game {
  runGame :: StateT GameState (IO) a
  } deriving (Monad, MonadIO, MonadState GameState)
             
runMyGame :: MyGame a -> GameState -> IO (a, GameState)
runMyGame k myState = runStateT (runGame k) myState




data DrawOrder = DoBackground 
               | DoForeground
               | DoSprite
               deriving (Show, Ord, Eq)
                 
data GameEvent = SDLGameEvent Event |
                 GameEvent String
                 

-- drawable type class
class Drawable a where
  drawOrder :: a -> DrawOrder
  surface :: a -> Maybe Surface
  loadSurface :: a -> Surface -> a
  filePath :: a -> String
  coordinates :: a -> Maybe Rect
  isVisible :: a -> Bool
  tag :: a -> String
  processEvents :: a -> [GameEvent] -> Double -> [AnyDrawable] -> ScreenConfig -> a
  


--existential type for polymorphic Drawable types
data AnyDrawable = forall a. Drawable a => AnyDrawable a

instance Drawable AnyDrawable where
  drawOrder (AnyDrawable a) = drawOrder a
  surface (AnyDrawable a) = surface a
  loadSurface (AnyDrawable a) newSurface = AnyDrawable (loadSurface a newSurface)
  filePath (AnyDrawable a) = filePath a
  coordinates (AnyDrawable a) = coordinates a
  isVisible (AnyDrawable a) = isVisible a
  tag (AnyDrawable a) = tag a
  processEvents (AnyDrawable a) gameEvents tStep drawableList sConfig = AnyDrawable (processEvents a gameEvents tStep drawableList sConfig)


instance Eq AnyDrawable where
  (==) a b = (drawOrder a) == (drawOrder b)
  (/=) a b = not $ a == b

instance Ord AnyDrawable where
  (<) a b = (drawOrder a) < (drawOrder b)
  (<=) a b = (drawOrder a) <= (drawOrder b)
  (>) a b = (drawOrder a) > (drawOrder b)
  (>=) a b = (drawOrder a) >= (drawOrder b)
  max a b = if a >= b then a else b
  min a b = if a < b then a else b
  compare a b = compare (drawOrder a) (drawOrder b)


-- paddle Type
data Paddle = Paddle {
  paddleDrawOrder :: DrawOrder,
  paddleCoordinates :: Rect, -- | top left corner
  paddleImageFile :: String,
  paddleImageWidth :: Int,
  paddleImageHeight :: Int,
  paddleSurface :: Maybe Surface,
  paddleVisible :: Bool,
  paddlePosition :: Vector, -- | middle of the image
  paddleVelocity :: Vector,
  paddleTag :: String
  }

-- ball type
data Ball = Ball {
  ballDrawOrder :: DrawOrder,
  ballCoordinates :: Maybe Rect,
  ballImageFile :: String,
  ballSurface :: Maybe Surface,
  ballVisible :: Bool,
  ballPosition :: Vector,
  ballVelocity :: Vector,
  ballShape :: Shape,
  ballTag :: String
}