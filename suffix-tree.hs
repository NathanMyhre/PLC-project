import Data.List

type Edge = (String, STree)

 -- Dumb suffix tree that does not know about its alphabet 
data STree = Leaf | Node [Edge]
  deriving Show



-- Adds the terminal character, "$" to a string
-- Need to add the terminal character somewhere else. Maybe within mkTree?
addTerminal ::  String -> String
addTerminal s = s ++ "$"

-- Do we need to insert alphabetically? for now, I am not.
-- Helper function that adds an edge to the graph
insertEdge :: String -> STree -> STree
insertEdge s Leaf = Node [(s, Leaf)]
insertEdge s (Node xs) = Node (xs ++ (s , Leaf) : [])

{- Split an edge into a subtree, after a given prefix.
   Assume that at least 1 letter from string matches, and will always
   end with "$"

   Case 1: Edge to split ends in a leaf:
     In this case, we should just traverse the leaf and insert the
     suffix at the first point where input string and edge string
     are different. e.g. "ab$" and ("abcab$", Leaf) will become edge:
     ("ab", Node [("$", Leaf), ("cab$", Leaf)])

   Case 2: Edge to split contains part
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
