doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
hoge = "Hoge"

-- リストとか
a = take 24 [13, 26..]
b = take 10 (cycle [1,2,3])
c = take 10 (repeat 5)
d = replicate 3 10
modBetween50and100 xs = [x | x <- [50..100], x `mod` xs == 3]
funcA = [x * y | x <- [1, 2, 7], y <- [1, 2, 7]]
length' xs = sum [1 | _ <- xs]

