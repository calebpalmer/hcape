module Hcape.Geometry.Trigonometry where

import Hcape.Geometry.Common (GReal)

fromDegrees :: GReal -> GReal
fromDegrees d = 
  d * pi / 180
  
toDegrees :: GReal -> GReal
toDegrees d =
  d * 180 / pi