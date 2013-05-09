module Hcape.Physics where

-- to implement:  y intersect
-- to implement:  x intersect 
-- to implement:  vectors intersect? at what point? Linear Combination

import Time

type PReal = Double

data Point = Point {pointX :: PReal,
                    pointY :: PReal,
                    pointZ :: PReal
                   } deriving Show
             
data Slope = Slope {slopeX :: PReal,
                    slopeY :: PReal,
                    slopeZ :: PReal
                   } deriving Show
             
     
data Vector = Vector {vectorPosition :: Point,
                      vectorSlope :: Slope,
                      vectorVelocity :: PReal
                     } deriving Show
              

clockTimeDiffToSeconds :: ClockTime -> ClockTime -> PReal
clockTimeDiffToSeconds c1 c2 =
  (fromIntegral . tdPicosec $ (diffClockTimes c2 c1)) / 1000000000000.0
  
calculateSlope :: Point -> Point -> PReal
calculateSlope p1 p2 = ((pointY p2) - (pointY p1)) / ((pointX p2) - (pointX p1))

calculateOrthogonalSlope :: Point -> Point -> PReal
calculateOrthogonalSlope p1 p2 = (-1) / (calculateSlope p1 p2)

isSlopeOrthogonol :: PReal -> PReal -> Bool
isSlopeOrthogonol s1 s2 = if s1 * s2 == -1 then True
                     else False
                          
-- integrate vector using Euler integration
integrateVector :: PReal -> Vector -> Vector
integrateVector ts v = v { vectorPosition = Point newX newY newZ }
  where newX = (pointX . vectorPosition $ v) + ((vectorVelocity v) * (slopeX . vectorSlope $ v) * ts)
        newY = (pointY . vectorPosition $ v) + ((vectorVelocity v) * (slopeY . vectorSlope $ v) * ts)
        newZ = (pointZ . vectorPosition $ v) + ((vectorVelocity v) * (slopeY . vectorSlope $ v) * ts)
  