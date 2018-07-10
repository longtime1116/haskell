doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
hoge = "Hoge"

a = take 24 [13, 26..]
b = take 10 (cycle [1,2,3])
c = take 10 (repeat 5)
d = replicate 3 10


