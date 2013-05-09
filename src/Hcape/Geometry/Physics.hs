module Hcape.Geometry.Physics where

import Geometry.Vector
import Geometry.Common

-- | integrate a vector by a timestep using Euler integration
integrateVector :: Vector -- | the position vector
                   -> Vector -- | the velocity Vector
                   -> GReal -- | the elapsed time
                   -> Vector -- | the resulting vector
integrateVector p v ts =
  p + (scale v ts)
