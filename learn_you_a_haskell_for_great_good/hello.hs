-- $ ghc --make hello
-- main = putStrLn "hello, world"

-- main = do
--     putStrLn "Hello, what's your name?"
--     name <- getLine
--     putStrLn $ "Hello, " ++ name


-- let は pure な式に名前を束縛するときに使う
-- import Data.Char
--
-- main = do
--     putStrLn "What's your first name?"
--     firstName <- getLine
--     putStrLn "What's your last name?"
--     lastName <- getLine
--     let bigFirstName = map toUpper firstName
--         bigLastName = map toUpper lastName
--     putStrLn $ "Hello, " ++ bigFirstName ++ " " ++ bigLastName ++ "."


-- return () は引数で渡した pure な値から I/O アクションを作り出すもの
-- main = do
--     putStrLn "What's your name?"
--     name <- getLine
--     if null name
--         then return ()
--         else do
--             putStrLn $ reverseWords name
--             main
-- reverseWords :: String -> String
-- reverseWords = unwords . map reverse . words


-- print = putStrLn . show
-- main = do
--     print True
--     print 1.1
--     print [1,2,3]


-- when
-- import Control.Monad
-- main = do
--         putStrLn "Say YES!"
--         input <- getLine
--         when (input == "YES") $ do
--           putStrLn "Exactly!"
--           main


-- seqence は I/O アクションのリストを受け取り、それらを順に実行する I/O アクションを返す
-- この I/O アクションが生成する結果は、それぞれの I/O アクションを実行した結果である。
-- <- を使うと結果を名前に束縛するので、↓の例だと rs は I/O アクションの実行結果の配列となる

-- main = do
--         a <- getLine
--         b <- getLine
--         c <- getLine
--         putStrLn $ a ++ b ++ c

main = do
        rs <- sequence [getLine, getLine, getLine]
        print rs

