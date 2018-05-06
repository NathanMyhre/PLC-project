import Data.List

type Edge = (String, STree)

 -- Dumb suffix tree that does not know about its alphabet 
data STree = Leaf | Node [Edge]
  deriving Show



-- Adds the terminal character, "$" to a string
-- Need to add the terminal character somewhere else. Maybe within mkTree?
addTerminal ::  String -> String
addTerminal s = s ++ "$"

-- Helper function that adds an edge to the graph
insertEdge :: String -> STree -> STree
insertEdge s Leaf = Node [(s, Leaf)]
insertEdge s (Node xs) = Node (xs ++ (s , Leaf) : [])

{- Split an edge into a subtree, given a partial matching substring, ending
   in a leaf
-}
splitEdge :: String -> Edge -> Edge
splitEdge [] e = ([], Leaf)
splitEdge s (s', n) = let (x, y) = (uncommonSuffix s s') in
                      ((commonPrefix s s'), Node [(y, n), (x, Leaf)])

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
--------------------------------------------------------------------------------

-- Searches edges leaving a single node for a desired prefix.
-- Just a dumb search algorithm, we're only using it with 4 letters so
-- no need for efficiency.
findPrefix :: String -> STree -> Bool
findPrefix s Leaf = False
findPrefix s (Node []) = False
findPrefix s (Node ((prefix, tree) : edges) ) =
           if (head s) == (head prefix) then True
           else (findPrefix s (Node edges) )

-- Finds a path within tree, given a String
-- note: Make sure insertEdge retains the old node too.
findAndInsert :: String -> STree -> STree
findAndInsert s (Node []) = insertEdge s (Node [])
findAndInsert s (Node ((s', Leaf) : []) ) =
                if (commonPrefix s s') == [] then
                  Node ((s', Leaf) : (s, Leaf) : [])
                  else (Node ((splitEdge s (s', Leaf)) : []))
findAndInsert s (Node ((s', Leaf) : (s'', n) : next) ) =
                if (commonPrefix s s') == [] then     -- check next node 
                  Node ((s', Leaf) : (s'', findAndInsert s n) : next)
                  else (Node ((splitEdge s (s', Leaf)) : next))
findAndInsert s (Node ((s', n) : [] ) ) =
                if commonPrefix s s' == [] then
                  insertEdge s (Node ((s', n) : []))
                  else (Node ((s', findAndInsert s n) : []))
                  
                  


mkTree :: String -> STree
mkTree [] = Node []
mkTree (x : xs) = Leaf

-- Helper for Show functions ---------------------------------------------------

{--- Convert edges to String
-- Convert nodes to String

showSTree :: Show (STree) => STree -> String
showSTree tree = ""

instance Show (STree) where
  show t = showSTree t
-}

testEdge :: Edge
testEdge = ("abcab$", Leaf)

testTree :: STree
testTree = Node [("ab", Node [("ca$", Leaf), ("$", Leaf)]),
                 ("bca$", Leaf), ("ca", Leaf)]

testTree1 :: STree
testTree1 = Node []

testString = "abcab$"

a = findAndInsert "abcab$" testTree1
b = findAndInsert "bcab$" (Node [("abcab$", Leaf)])
c = findAndInsert "cab$" b
d = findAndInsert "ab$" c
e = findAndInsert "b$" d
f = findAndInsert "$" e
