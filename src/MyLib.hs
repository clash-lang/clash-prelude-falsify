module MyLib (someFunc) where

import Test.Falsify ()

someFunc :: IO ()
someFunc = putStrLn "someFunc"
