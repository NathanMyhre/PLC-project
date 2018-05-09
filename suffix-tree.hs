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
findAndInsert (s, n) node = case node of
  Node [] -> insertEdge (s, n) node
  Node ( (str, tree) : next) ->
    let listOfEdges = (map (findAndInsertE (s, n)) next) in
      if commonPrefix s str /= [] then
            Node ( (findAndInsertE (s, n) (str, tree)) : next)
      else Node ((str, tree) : listOfEdges)
           
findAndInsertE :: (String, Int) -> Edge -> Edge
findAndInsertE (s, n) (str, tree) = let commStr = commonPrefix s str in
  case tree of
    Leaf n -> if commStr /= [] then splitEdge (s, n) (str, tree)
              else (str, tree)
    Node t -> let (x, y) = uncommonSuffix s str in
              if commStr /= [] then
                if y /= [] then splitEdge (s, n) (str, tree)--prefix not eaten
                else (commStr, (findAndInsert (s, n) tree))
              else (str, (findAndInsert (s, n) tree))
   
mkTree :: String -> STree
mkTree [] = Node []

-- Test variables --------------------------------------------------------------


testTree :: STree
testTree = Node [("ab",Node [("",Leaf 3),("cab",Leaf 0)]),("b",Node [("",Leaf 4),("cab",Leaf 1)]),("cab",Leaf 2)]

testTreeInc = Node [("abcab", Leaf 0)]

testTree1 :: STree
testTree1 = Node []

testString = "abcab$"

a = findAndInsert ("abcab$", 0) testTree1
b = findAndInsert ("bcab$", 1) (Node [("abcab$", Leaf 0)])
c = findAndInsert ("cab$", 2) b
d = findAndInsert ("ab$", 3) c
e = findAndInsert ("b$", 4) d
f = findAndInsert ("$", 5) e
