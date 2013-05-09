-- | Provides circle datatype and functions
module Hcape.Geometry.Circle 
       (
         Circle(..)
        ,intersecting
         )where

import Hcape.Geometry.Common (GReal)
import Hcape.Geometry.Vector

data Circle =
  Circle {
    circX :: GReal -- | the x coord of center of circle
    ,circY :: GReal -- | the y coord of center of circle
    ,circRadius :: GReal -- | the radius of the circle
    } deriving (Eq, Show)
               
-- | detects whether two circles are intersecting
intersecting :: Circle -> Circle -> Bool
intersecting (Circle ax ay ar) (Circle bx by br) =
  let rDistance = abs $ distance (Vector ax ay 0) (Vector bx by 0)
      radiaiCombined = ar + br
  in if rDistance <= radiaiCombined then True
     else False
  