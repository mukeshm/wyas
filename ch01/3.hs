module Main where

main :: IO ()
main = do putStrLn "Enter your name : "
          name <- getLine
          putStrLn $ "hello " ++ name 
