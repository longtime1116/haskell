-- $ ghc --make hello
-- main = putStrLn "hello, world"

main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Hello, " ++ name
