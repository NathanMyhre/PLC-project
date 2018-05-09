import SuffixTreeAlt

testTree = buildTree "abcab"
testTree2 = buildTree "banananana" -- 4 repeats of "na"

a :: [(String, SuffixTree)]
a = [("abc", Node [("def",Leaf 0)]), ("bce", Leaf 1), ("cde", Leaf 2)]

-- Helpers to extract Tree, and String from an edge.
getTree :: (String, SuffixTree) -> SuffixTree
getTree (a, b) = b

getString :: (String, SuffixTree) -> String
getString (a, b) = a

join :: [[a]] -> [a]
join [] = []
join (x : xs) = x ++ join xs

dfsPractice :: SuffixTree -> [Int]
dfsPractice (Leaf n) = (n : [])
dfsPractice (Node [] ) = []
dfsPractice (Node ((str, tree) : next)) = 
  let x = (map getTree next) in
    (dfsPractice tree) ++ join( map dfsPractice x)

numberNodes :: Int -> SuffixTree  -> Int
numberNodes n (Leaf n) = 0


{-Finding Maximum Unique matching substrings.
  - Take two strings and build a Suffix Tree out of their concatenation
  - Do a DFS search on the tree
  
