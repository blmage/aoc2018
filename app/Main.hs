module Main where

import qualified Day01
import qualified Day02
import qualified Day03


printFailable :: Show a => Either String a -> IO ()
printFailable = either putStrLn print


main :: IO ()
main = do
    putStrLn "Day 01:"
    putStr "* Answer 1: " *> Day01.answer1 >>= print
    putStr "* Answer 2: " *> Day01.answer2 >>= print
    putStrLn "Day 02:"
    putStr "* Answer 1: " *> Day02.answer1 >>= print
    putStr "* Answer 2: " *> Day02.answer2 >>= print
    putStrLn "Day 03:"
    putStr "* Answer 1: " *> Day03.answer1 >>= printFailable
    putStr "* Answer 2: " *> Day03.answer2 >>= printFailable
