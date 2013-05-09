-- | Provides matrix datatype and function
-- Matrices are just containers of Vectors in this case
module Hcape.Geometry.Matrix where

import Hcape.Geometry.Vector
import Hcape.Geometry.Common

newtype Matrix = Matrix [Vector]

-- | insert a Vector to the beginning of the matrix
matrixInsert :: Matrix -> Vector -> Matrix
matrixInsert m1@(Matrix l) v =
  Matrix $ v:l
  
-- | insert a Vector to the beginning of the matrix
matrixAppend :: Matrix -> Vector -> Matrix
matrixAppend m1@(Matrix l) v =
  Matrix $ l ++ [v]

-- | join two matrices together
matrixJoin :: Matrix -> Matrix -> Matrix
matrixJoin (Matrix l1) (Matrix l2) =
  Matrix (l1 ++ l2)
  
-- | test if to Matrices are of the same order
equalOrder :: Matrix -> Matrix -> Bool
equalOrder (Matrix l1) (Matrix l2) =
  length l1 == length l2
  
-- | gets the order of the matrix
matrixOrder :: Matrix -> Int
matrixOrder (Matrix l) =
  length l

-- | Matrix addition
-- throws an error if the matrices are not of the same order
matrixAdd :: Matrix -> Matrix -> Matrix
matrixAdd m1@(Matrix l1) m2@(Matrix l2)
  | not $ equalOrder m1 m2 = error "Cannot add matrices of different orders"
  | length l1 == 0 = m2
  | length l2 == 0 = m1
matrixAdd m1@(Matrix (x:xs)) m2@(Matrix (y:ys)) = 
  (Matrix [(x + y)]) `matrixJoin` (matrixAdd (Matrix xs) (Matrix ys))
  

-- | Matrix subtraction
-- throws an error if the matrices are not of the same order
matrixSubtract :: Matrix -> Matrix -> Matrix
matrixSubtract m1@(Matrix l1) m2@(Matrix l2)
  | not $ equalOrder m1 m2 = error "Cannot subtract matrices of different orders"
  | length l1 == 0 = m2
  | length l2 == 0 = m1
matrixSubtract m1@(Matrix (x:xs)) m2@(Matrix (y:ys)) = 
  (Matrix [(x - y)]) `matrixJoin` (matrixSubtract (Matrix xs) (Matrix ys))
  
-- | mulply a matrix by a ascaler
scalarMultiply :: Matrix -> GReal -> Matrix
scalarMultiply (Matrix l) s =
  Matrix $ map ((flip scale) s) l