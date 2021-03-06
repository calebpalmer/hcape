-- | Provides vector datatype and functions
module Hcape.Geometry.Vector where

import Hcape.Geometry.Common (GReal)

data Vector =  
  Vector { 
    vecX :: GReal, -- | the x coordinate
    vecY :: GReal, -- | the y coordinate
    vecZ :: GReal  -- | the z coordinate
    } deriving Show
               
data PVector = PVector {
  vecMagnitude :: GReal,  -- | the magnitude of the vector
  vecAngle :: GReal -- | the angle of the vector
  } deriving Show
  
instance Eq Vector where
  (==) a b =
    vecX a == vecX b
    && vecY a == vecY b 
    && vecZ a == vecZ b-- recursive fold

  (/=) a b = not $ a == b
  
-- | Adds two vectors together
vectorAdd :: Vector -- | first vector to add
             -> Vector -- | second vector to add
             -> Vector -- | vectors added together
vectorAdd a b =
  let x = vecX a + vecX b
      y = vecY a + vecY b
      z = vecZ a + vecZ b
  in Vector x y z

-- | Multiply two vectors together
vectorMultply :: Vector -> Vector -> Vector
vectorMultply a b =
  let x = vecX a * vecX b
      y = vecY a * vecY b
      z = vecZ a * vecZ b
  in Vector x y z

-- | subtract one vector from another
vectorSubtract :: Vector -> Vector -> Vector
vectorSubtract (Vector ax ay az) (Vector bx by bz) = 
  Vector (bx - ax) (by - ay) (bz - az)

-- | negate a vector
vectorNegate :: Vector -> Vector
vectorNegate a = 
  let x = (negate . vecX) a
      y = (negate . vecY) a
      z = (negate . vecZ) a
  in Vector x y z
     
-- | return absolute value of a vector
vectorAbs :: Vector -> Vector
vectorAbs a =
  let x = (abs . vecX) a
      y = (abs . vecY) a
      z = (abs . vecZ) a
  in Vector x y z

-- | return the signum of a vector
vectorSignum :: Vector -> Vector
vectorSignum a = a

instance Num Vector where
  (+) = vectorAdd
  (-) = vectorSubtract
  (*) = vectorMultply
  negate = vectorNegate
  abs = vectorAbs
  signum = vectorSignum
  fromInteger = \_ -> Vector 0.0 0.0 0.0
  
--  | get the distance between two vectors
-- pythagoean theorem
-- c^2 = a^2 + b^2
-- c = sqrt((x2 - x1)^2 + (y2 - y1)^2)
distance :: Vector -> Vector -> GReal
distance (Vector ax ay _) (Vector bx by _) =
  let xs = bx - ax
      ys = by - ay
  in sqrt $ xs * xs + ys * ys
     
-- | Scale the vector by a certain factor
scale :: Vector -> GReal -> Vector
scale (Vector ax ay az) s =
  Vector (s * ax) (s * ay) (s * az)
  
-- | gets magnitude of a Vector (hypoteneuse)
magnitude :: Vector -> GReal
magnitude v =
  distance (Vector 0 0 0) v
  
-- | returns a Normalized copy of the provided vector (can be used a slope vector)
normalize :: Vector -> Vector
normalize v =
  scale v $ 1 / mag
  where mag = magnitude v

-- | calculate the dot product of two vectors
dotProduct :: Vector -> Vector -> GReal
dotProduct (Vector ax ay az) (Vector bx by bz) =
  ax * bx + ay * by + az * bz
  
-- | calculate the cross product of two vectors
crossProduct :: Vector -> Vector -> Vector
crossProduct (Vector ax ay az) (Vector bx by bz) =
  Vector newx newy newz
  where newx = ay * bz - az * by
        newy = az * bx - ax * bz
        newz = ax * by - ay * bx
  
-- | gets the angle between two vectors
-- A dot B = ||a|| ||b||cos theta
-- inverse cos * (A dot B) / ||a|| ||b||
angle :: Vector -> Vector -> GReal
angle v1 v2 =
  acos $ (v1 `dotProduct` v2) / ((magnitude v1) * (magnitude v2))
  
-- | returns normalized vector of new vectors
surfaceNormal :: Vector -> Vector -> Vector
surfaceNormal v1 v2 =
  normalize crossProd
  where crossProd = crossProduct v1 v2
  
-- | converts a vector in polar coordinates to cartesion coordinates
polarToCartesian :: PVector -> Vector
polarToCartesian (PVector mag ang) = 
  Vector x y z
  where x = mag * cos ang
        y = mag * sin ang
        z = 0