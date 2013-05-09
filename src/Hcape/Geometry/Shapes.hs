module Hcape.Geometry.Shapes where

import Hcape.Geometry.Vector
import Hcape.Geometry.Common

data Shape =
  Line {
    lineStart :: Vector -- | starting point of the line
    ,lineEnd :: Vector -- | end poing of the line
    } |
  Circle {
    circVector :: Vector -- | the center of the circle
    ,circRadius :: GReal -- | the radius of the circle
    } |
  Rectangle {
    rectBottomLeft :: Vector -- | Bottom left corner of the Rectangle
    ,rectTopRight :: Vector -- | Top right corner of the Rectangle
    } |
  Polygon {
    vertices :: [Vector] -- | list of vertices that make up the Polygon
    }
  
-- | determines whether two shapes are colliding
collides :: Shape -> Shape -> GReal -> Bool
collides (Circle cp cr) (Circle cp2 cr2) buf = 
  let rDistance = abs $ distance cp cp2
      radiaiCombined = cr + cr2
  in if (rDistance + buf) <= radiaiCombined 
        || (rDistance - buf) <= radiaiCombined then True
     else False
  