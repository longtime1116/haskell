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

-- :t read    # show :: Show a => a -> String
-- read "8.2" + 3.8 + read "1.0"
-- read "5" :: Int
-- read "5" :: Float
-- read "(1, 'a')" :: (Int, Char)
-- read "True" :: Bool
-- [read "True", True, False, False]


--[LT .. GT]
--['A'..'z']
--succ 'B'
--pred 'B'

-- :t minBound  # minBound :: Bounded a => a
-- maxBound :: Int
-- minBound :: Int
-- maxBound :: Char
-- minBound :: Char
-- maxBound :: Bool
-- minBound :: Bool
-- maxBound :: (Bool, Int, Char)


-- :t 20      # 20 :: Num p => p
-- 20 :: Double


-- fromIntegral (length [1,2,3]) + 3.2


lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're outof luck, pal!"


factorial2 :: Int -> Int
factorial2 0 = 1
factorial2 n = n * factorial2 (n - 1)

capital :: String -> Char
capital "Albert" = 'a'
capital "x" = 'x'
capital x = '!'

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one elemet: " ++ show x
tell (x:y:[]) = "The list has two elemets: " ++ show x ++ ", " ++ show y
tell (x:y:_) = "The list has more than two elemets: " ++ show x ++ ", " ++ show y ++ ", and etc..."

firstLetter :: String -> String
firstLetter "" = "Empty string."
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] -- x は Char なので、[Char] にすると連結できる

-- max' :: Ord -> Ord -> Ord   <- このようにできない理由は、Ord 型の値というわけではなく、
-- Ord を実装したより具体的な何かを指し示しているから?
-- 一方 Double -> Double -> Double とかだと、Double型の値そのものなのでOKなのでは
-- Ord は型クラスであり、Doubleは型
max' :: (Ord a) => a -> a -> a
max' a b
  | a <= b = b
  | otherwise = a

bmi :: Double -> Double -> Double
bmi weight height = weight / height ^ 2

bmiTell :: Double -> Double -> String
bmiTell weight height
  | (bmi weight height) <= 18.5 = "A"
  | (bmi weight height) <= 25.0 = "B"
  | (bmi weight height) <= 30.0 = "C"
  | otherwise = "F"

bmiTell' :: Double -> Double -> String
bmiTell' weight height
  | bmi <= skinny = "A"
  | bmi <= normal = "B"
  | bmi <= fatty  = "C"
  | otherwise = "F"
  where bmi = weight / height ^ 2  -- インデントがずれるとエラーになる
        skinny = 18.5               -- スコープを汚染しない
        normal = 25.0
        fatty = 30.0

bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
  | bmi <= skinny = "A"
  | bmi <= normal = "B"
  | bmi <= fatty  = "C"
  | otherwise = "F"
  where bmi = weight / height ^ 2  -- インデントがずれるとエラーになる
        (skinny, normal, fatty) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^2


calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (weight, height) <- xs, let bmi = weight / height ^ 2, bmi > fatty]
  where fatty = 30.0

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h     -- let bindings in expression で let 式
        topArea = pi * r ^ 2
    in sideArea + topArea * 2


describeString :: String -> String
describeString ls = "The list is "
                  ++ case ls of [] -> "empty."
                                [x] -> "a singleton list."
                                (x:_) -> "a longer list, and the first is " ++ [x]


describeList :: [a] -> String
describeList ls = "The list is " ++ what ls
  where what [] = "empty."
        what [x] = "a singleton list."
        what xs = "a longer list."


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "error!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n-1) x

take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' a = [a] ++ repeat' a


zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs)
  | n == x = True
  | otherwise  = elem' n xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [a | a <- xs, a <= x] ++ [x] ++ quicksort [a | a <- xs, a >= x]

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let smallOrEqual = [a | a <- xs, a <= x]
        large = [a | a <- xs, a >= x]
    in quicksort smallOrEqual ++ [x] ++ quicksort large


-- Int 型の値を受け取り、(Int->(Int->Int)) の型を返す
--    -> Int 型の値を受け取り、(Int->Int) の型を返す
--      -> Int 型の値を受け取り、Int の型を返す
multiThree :: Int -> (Int -> (Int -> Int))
multiThree x y z = x * y * z

multiFour :: Int -> Int -> (Int -> (Int -> Int))
multiFour a b c d = (multiThree a) b c * d

multiTwoWithNine = multiThree 9   -- ghci では let で宣言するとエラーにならない
-- multiTwoWithNine 2 3


-- compare :: (Ord a) => a -> a -> Ordering なので
compareWithHundred :: Int -> Ordering
compareWithHundred x = x `compare` 100


compareToHundred :: Int -> Ordering
compareToHundred = (100 `compare`)

compareToHundred' :: Int -> Ordering
compareToHundred' = (compare 100)


divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f(f x)


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
flip' f x y = f y x
-- hoge = flip' compare
-- hoge 100 2 => LT
-- geho = flip' compare 100 2

-- map
map' :: (t -> a ) -> [t] -> [a]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- filter
filter' :: (a -> Bool ) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

largestDivisible = head (filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0

takeDivisible = takeWhile (> 0) (filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0

-- コラッツ列
-- 任意の自然数から開始する
-- 数が1ならば終了
-- 数が偶数ならば2で割る
-- 数が奇数ならば3倍して1足す
-- 新しい値でこのアルゴリズムを繰り返す
-- どんな数字を初期値としてとっても 1 に到達すると予想されている
-- 1〜100までの数値でコラッツ列が15以上になるものを列挙する
collatz :: Integral a => a -> [a]
collatz 1 = [1]
collatz x
  | even x = x : collatz (x `div` 2)
  | odd x = x : collatz (x * 3 + 1)

numberOfLongCollatz :: Int -> Int
numberOfLongCollatz n = length(filter isLong (map collatz [1..100]))
  where isLong xs = length(xs) >= n
longCollatz :: Integral a => Int -> [[a]]
longCollatz n = filter isLong (map collatz [1..100])
  where isLong xs = length(xs) >= n
longCollatzWithLambda :: Integral a => Int -> [[a]]
longCollatzWithLambda n = filter (\xs -> length(xs) >= n) (map collatz [1..100])

suml :: (Foldable t, Num a) => t a -> a
suml xs = foldl (\acc x -> acc + x) 0 xs
suml' :: (Foldable t, Num a) => t a -> a
suml' = foldl (+) 0
sumr :: (Foldable t, Num a) => t a -> a
sumr xs = foldr (\x acc -> x + acc) 0 xs

mapr :: (Foldable t, Num a) => t a -> [a]
mapr xs = foldr (\x acc -> x + 1 : acc) [] xs

-- elem'' 2 [2,3..] みたいに無限配列に対して使える、True ならば。
elem'' :: (Foldable t, Eq a) => a -> t a -> Bool'
elem'' y ys = foldr (\x acc -> if x == y then True else acc) False ys

