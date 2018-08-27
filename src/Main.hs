module Main where

import qualified Tree as Tree

main :: IO ()
main =
  do print (Tree.toList (Tree.fromList "hello, world"))
     print (Tree.toList (Tree.fromList [10, 9 .. 1]))
