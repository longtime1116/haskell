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
main = do
    putStrLn "What's your name?"
    name <- getLine
    if null name
        then return ()
        else do
            putStrLn $ reverseWords name
            main
reverseWords :: String -> String
reverseWords = unwords . map reverse . words
