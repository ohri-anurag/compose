module Main where

import qualified Data.ByteString as B

import Lib

main :: IO ()
main = do
  someFunc

  B.writeFile "test.wav" $ B.pack [0, 1]
