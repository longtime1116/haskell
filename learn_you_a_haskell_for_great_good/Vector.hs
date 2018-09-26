module Vector
( Vector(..)
, vplus
) where

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector x1 x2 x3) `vplus` (Vector y1 y2 y3) = Vector (x1 + y1) (x2 + y2) (x3 + y3)
