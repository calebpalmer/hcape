module Hcape.Video 
       (
         renderObject
         ,getSurfaceDimensions
         ,renderScreen
         ,loadResource
       )
       where

import Hcape.Types

import qualified Graphics.UI.SDL.Video as SDLV
import Graphics.UI.SDL.Types
  
renderObject :: Surface -> AnyDrawable -> IO ()
renderObject mainSurf robj
  = case surface robj of Just surf -> do
                           _ <- SDLV.blitSurface surf Nothing mainSurf $ coordinates robj
                           return ()
                         Nothing -> do
                           putStrLn "Error: surface not loaded"
                           
renderScreen :: Surface -> IO ()
renderScreen s = SDLV.flip s

loadResource :: AnyDrawable -> IO Surface
loadResource robj = do
  sdlSurface <- SDLV.loadBMP $ filePath robj
  pixelFormat <- SDLV.getVideoSurface >>= \s -> return . surfaceGetPixelFormat $ s
  pixel <-SDLV.mapRGB pixelFormat 0 255 255
  _ <- SDLV.setColorKey sdlSurface [SrcColorKey, RLEAccel] pixel
  optSurface <- SDLV.displayFormat sdlSurface
  return optSurface
  
-- | get the width and height of the surface
getSurfaceDimensions :: Surface -> (Int, Int)
getSurfaceDimensions s = 
  let width = surfaceGetWidth s
      height = surfaceGetHeight s
  in (width, height)
