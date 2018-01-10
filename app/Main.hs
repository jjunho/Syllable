{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Syllable.Port

main :: IO ()
main = putStrLn $ strSyllables "juliano"
