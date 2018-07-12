doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
hoge = "Hoge"

-- リストとか
a1 = take 24 [13, 26..]
a2 = take 10 (cycle [1,2,3])
a3 = take 10 (repeat 5)
a4 = replicate 3 10
modBetween50and100 xs = [x | x <- [50..100], x `mod` xs == 3]
funcA = [x * y | x <- [1, 2, 7], y <- [1, 2, 7]]
length' xs = sum [1 | _ <- xs]

-- タプル
b1 = [(1, "a"), (3, "b"), (5, "c")]
--- 型が違うとエラーになる
-- e_error = [(1, 2), (3, 4), (5, 6), (7)]
b2 = fst (1, "a")
b3 = snd (1, "a")
b4 = zip [1, 2, 3] ["a", "b", "c"]
b5 = zip [1, 2, 3, 4, 5] ["a", "b", "c"]
-- 直角三角形
b6 = take 10 [[c, b, a] | a <- [1..], b <- [1..a], c <- [1..b],
                          a^2 == b^2 + c^2]
rightAngleTriangle x = take x [[c, b, a] | a <- [1..],
                                           b <- [1..a],
                                           c <- [1..b],
                                           a^2 == b^2 + c^2]

-- product は 要素を全て掛け合わせる
factorial :: Integer ->Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r
circumference' :: Double -> Double
circumference' r = 2 * pi * r

-- :t head    # head :: [a] -> a

-- :t (==)    # (==) :: Eq a => a -> a -> Bool

-- :t (>)     # (>) :: Ord a => a -> a -> Bool
-- "ABC" < "BCD"
-- "ABC" `compare` "BCD"
-- "BCD" `compare` "ABC"

-- :t show    # show :: Show a => a -> String
-- show True

