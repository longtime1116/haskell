module Geometry2.Cuboid
( volume
, area
) where

volume :: Float -> Float -> Float -> Float
volume a b c = rectArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = rectArea a b * 2 + rectArea b c * 2 + rectArea c a * 2

rectArea :: Float -> Float -> Float
rectArea a b = a * b