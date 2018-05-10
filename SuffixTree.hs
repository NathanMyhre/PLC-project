module SuffixTree where

import Data.List

type Edge = (String, STree)

 -- Dumb suffix tree that does not know about its alphabet 
data STree = Leaf Int | Node [Edge]
  deriving Show

-- Adds the terminal character, "$" to a string
-- Need to add the terminal character somewhere else. Maybe within mkTree?
addTerminal ::  String -> String
addTerminal s = s ++ "$"

-- Helper function that adds an edge to the graph
insertEdge :: (String, Int) -> STree -> STree
insertEdge (s, n) (Leaf n') = Node [(s, Leaf n)]
insertEdge (s, n) (Node xs) = Node (xs ++ (s , Leaf n) : [])

{- Split an edge into a subtree, given a partial matching substring, ending
   in a leaf
-}
splitEdge :: (String, Int) -> Edge -> Edge
splitEdge ([], n) anything = ([], Leaf n)
splitEdge (str, n) (str', subtree) =
  let (x, y) = (uncommonSuffix str str') in
    ((commonPrefix str str'), Node [(x, Leaf n), (y, subtree)])

-- helpers for splitEdge -------------------------------------------------------
-- Find common prefix between two strings
commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix [] _ = []
commonPrefix _ [] = []
commonPrefix (x : xs) (y : ys) = if x == y then (x : (commonPrefix xs ys))
                             else []
-- Return the uncommon suffixes 
uncommonSuffix :: Eq a => [a] -> [a] -> ([a], [a])
uncommonSuffix [] b = ([], b)
uncommonSuffix a [] = (a, [])
uncommonSuffix (x : xs) (y : ys) = if x == y then (uncommonSuffix xs ys)
                                   else ( (x : xs), (y : ys) )

-- Finds a path within tree, given a String
-- note: Make sure insertEdge retains the old node too.

findAndInsert :: (String, Int) -> STree -> STree
findAndInsert ([], n) node = node
findAndInsert (s, n) node = case node of
  Node [] -> insertEdge (s, n) node
  Node ( x : xs ) ->
    if commonPrefix s (getString x) /= [] then
      Node ((findAndInsertE (s, n) x):xs)
    else combineNodes (Node (x:[])) (findAndInsert (s, n) (Node xs))
           
findAndInsertE :: (String, Int) -> Edge -> Edge
findAndInsertE (s, n) (str, tree) = let commStr = commonPrefix s str in
  case tree of
    Leaf n' -> if commStr /= [] then splitEdge (s, n) (str, tree)
              else (str, tree)
    Node t -> let (x, y) = uncommonSuffix s str in
              if commStr /= [] then
                if y /= [] then splitEdge (s, n) (str, tree)--prefix not eaten
                else (commStr, (findAndInsert (s, n) tree))
              else (str, (findAndInsert (s, n) tree))

-- Holy crap is this inefficient, but I can't figure out another way.
-- Combines two STrees, in order to concatenate list of edges.
combineNodes :: STree -> STree -> STree
combineNodes (Node x) (Node y) = Node (x ++ y)

-- Extracts data from tree: intentionally strict for explicit purpose
getTree :: (String, STree) -> STree
getTree (a, b) = b

getString :: (String, STree) -> String
getString (a, b) = a

-- Applies any SuffixTree change to the Suffix Tree.
applyToTree :: (STree -> STree) -> Edge -> Edge
applyToTree f (s, t) = (s, (f t))

-- Adds the termination character, '$' to the tree
addTermChar :: STree -> STree
addTermChar (Leaf n) = Leaf n
addTermChar (Node []) = Node []
addTermChar (Node ( (str, tree) : next ) ) =
  let x = (map addTermCharE next) in 
    case tree of
      Leaf n -> Node ( ( (str ++ "$"), tree) : x)
      Node xs -> Node (( str, (addTermChar tree)) : x)

-- Helps add Termination character, '$' to tree.
addTermCharE :: Edge -> Edge 
addTermCharE (str, tree) = case tree of
  Leaf n -> ( (str ++ "$", tree) )
  Node xs -> ( str, (addTermChar tree) )

suffixes :: String -> [String]
suffixes s = case s of
  [] -> []
  (s : str) -> [(s : str)] ++ (suffixes str)

mkTree :: String -> STree
mkTree str = addTermChar (buildTree (reverse (suffixes str)) ( (length str) - 1))

buildTree :: [String] -> Int -> STree
buildTree [] n = Node []
buildTree (s : str) n =  findAndInsert (s, n) (buildTree str (n - 1))
