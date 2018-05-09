import SuffixTreeAlt

testTree = buildTree "abcab"
testTree2 = buildTree "banananana" -- 4 repeats of "na"

a :: [(String, SuffixTree)]
a = [("abc", Node [("def",Leaf 0)]), ("bce", Leaf 1), ("cde", Leaf 2)]

-- Depth first search practice, returns the Leaf numbers from the tree's leaves
dfsPractice :: SuffixTree -> [Int]
dfsPractice (Leaf n) = (n : [])
dfsPractice (Node [] ) = []
dfsPractice (Node ((str, tree) : next)) = 
  let x = (map getTree next) in
    (dfsPractice tree) ++ (foldr (++) [] (map dfsPractice x))

-- Finds the number of internal nodes in a Suffix Tree.
numberOfNodes :: Int -> SuffixTree  -> Int
numberOfNodes n (Leaf n') = 0
numberOfNodes n (Node []) = 0
numberOfNodes n (Node ( (str, tree) : next) ) =
  let x = (map getTree next) in
    1 + (numberOfNodes n tree) + (foldr (+) 0 (map (numberOfNodes n ) x)) 

-- Finds if a substring exists within a suffix tree and return it.
-- Returns a list of substrings, along with number of nodes it traversed.
findPath :: String -> SuffixTree -> Maybe [(String, Int)]
findPath (s : sx) Leaf n' = Nothing
findPath (s : sx) (Node ( (str, tree) : next) ) = []
  

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

{-Finding Maximum Unique matching substrings.
  - Take two strings and build a Suffix Tree out of their concatenation
  - Do a DFS search on the tree
  - Label each node so according to the substring leading to it.
  -}
