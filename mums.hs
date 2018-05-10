module Mums where

import SuffixTree

-- Depth first search, returns the Leaf numbers from the tree's leaves
dfsLeaves :: STree -> [Int]
dfLeaves (Leaf n) = (n : [])
dfsLeaves (Node [] ) = []
dfsLeaves (Node ((str, tree) : next)) = 
  let x = (map getTree next) in
    (dfsLeaves tree) ++ (foldr (++) [] (map dfsLeaves x))

-- Finds the number of internal nodes in a Suffix Tree.
numberOfNodes :: Int -> STree  -> Int
numberOfNodes n (Leaf n') = 0
numberOfNodes n (Node []) = 0
numberOfNodes n (Node ( (str, tree) : next) ) =
  let x = (map getTree next) in
    1 + (numberOfNodes n tree) + (foldr (+) 0 (map (numberOfNodes n ) x)) 

-- Finds if a substring exists within a suffix tree and return it.
-- Returns a list of substrings, along with number of nodes it traversed.
-- Could not finish this in time.
findPath :: String -> STree -> Maybe [(String, Int)]
findPath (s : sx) (Leaf n') = Nothing
findPath (s : sx) (Node ( (str, tree) : next) ) = Just []
  
{-Finding Maximum Unique matching substrings.
  - Take two strings and build a Suffix Tree out of their concatenation
  - Do a DFS search on the tree
  - Label each node so according to the substring leading to it.
  -}
