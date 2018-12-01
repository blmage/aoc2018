module Main where

import qualified Day01

main :: IO ()
main = do
    putStrLn "Day 01:"
    putStr "* Answer 1: " *> Day01.answer1 >>= print
    putStr "* Answer 2: " *> Day01.answer2 >>= print
