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

-- finds the number of internal nodes in a Suffix Tree.
numberOfNodes :: Int -> SuffixTree  -> Int
numberOfNodes n (Leaf n') = 0
numberOfNodes n (Node []) = 0
numberOfNodes n (Node ( (str, tree) : next) ) =
  let x = (map getTree next) in
    1 + (numberOfNodes n tree) + (foldr (+) 0 (map (numberOfNodes n ) x)) 


{-Finding Maximum Unique matching substrings.
  - Take two strings and build a Suffix Tree out of their concatenation
  - Do a DFS search on the tree
  - Label each node so according to the substring leading to it.
  -}
