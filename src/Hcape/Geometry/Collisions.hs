module Hcape.Geometry.Collisions  
       (checkMBRCollision
        ,Collision(..)
       )
       where

import Hcape.Geometry.Vector
import Hcape.Geometry.Shapes

data Collision = NoCollision |
                 GeneralCollision |
                 TopCollision |
                 RightCollision |
                 BottomCollision |
                 LeftCollision |
                 VectorCollision Vector

-- | Check to see if minimum bounding rectangles collide
checkMBRCollision :: Shape -> Shape -- | These must be Rectangles
                     -> Collision
checkMBRCollision (Rectangle (Vector l1 b1 _) (Vector r1 t1 _)) (Rectangle (Vector l2 b2 _) (Vector r2 t2 _)) =
  if (b1 < t2) || (b2 < t1) then NoCollision
  else 
    if (r1 < l2) || (r2 < l1) then NoCollision
    else GeneralCollision
checkMBRCollision _ _ = undefined -- do not check with any other shapes