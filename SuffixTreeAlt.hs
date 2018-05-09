{- Important: This SuffixTree file was obtained from the following URL:

   https://stackoverflow.com/questions/41333083/building-a-suffix-tree-by-inserting-each-suffix-in-haskell

  - We attempted to create our own data type, STree but could not resolve some
    issues with building the tree. That data type is located in suffix-tre.hs
  - At the end of this file are some helper functions written by us, labelled
    accordingly.
-}

module SuffixTreeAlt where

import Data.List (tails)
import Data.Maybe (maybeToList)
import Control.Arrow (first, second)
import Data.Map.Strict (Map, empty, insert, insertWith, assocs)

data SuffixTree
  = Leaf Int
  | Node [(String, SuffixTree)]
  deriving Show

data SuffixTrie
  = Leaf' Int
  | Node' (Map (Maybe Char) SuffixTrie)


buildTrie :: String -> SuffixTrie
buildTrie s = foldl go (flip const) (init $ tails s) (length s) $ Node' empty
  where
  go run xs i (Node' ns) = run (i - 1) $ Node' tr
    where tr = foldr loop (insert Nothing $ Leaf' (i - 1)) xs ns
  loop x run = insertWith (+:) (Just x) . Node' $ run empty
    where _ +: Node' ns = Node' $ run ns

buildTree :: String -> SuffixTree
buildTree = loop . buildTrie
  where
  loop (Leaf' i) = Leaf i
  loop (Node' m) = Node $ con . second loop <$> assocs m
  con (Just x, Node [(xs, tr)]) = (x:xs, tr) -- compress single-child nodes
  con n = maybeToList `first` n

doToTree :: ( a -> b ) -> (c, a) -> b
doToTree f (str, b ) = f b

-- Helper functions written by us ----------------------------------------------

-- Extracts data from tree
getTree :: (String, SuffixTree) -> SuffixTree
getTree (a, b) = b

getString :: (String, SuffixTree) -> String
getString (a, b) = a

-- Applies any SuffixTree change to the Suffix Tree.
applyToTree :: (SuffixTree -> SuffixTree) -> (String, SuffixTree) ->
  (String, SuffixTree)
applyToTree f (s, t) = (s, (f t))

-- Adds the termination character, '$' to the tree
addTermChar :: SuffixTree -> SuffixTree
addTermChar (Leaf n) = Leaf n
addTermChar (Node []) = Node []
addTermChar (Node ( (str, tree) : next ) ) =
  let x = (map addTermCharE next) in 
    case tree of
      Leaf n -> Node ( ( (str ++ "$"), tree) : x)
      Node xs -> Node (( str, (addTermChar tree)) : x)

addTermCharE :: (String, SuffixTree) -> (String, SuffixTree)
addTermCharE (str, tree) = case tree of
  Leaf n -> ( (str ++ "$", tree) )
  Node xs -> ( str, (addTermChar tree) )
