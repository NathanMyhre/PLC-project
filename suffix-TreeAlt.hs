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
