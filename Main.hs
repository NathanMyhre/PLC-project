import Mums
import SuffixTree

testTree = mkTree "abcab"

testTree2 = mkTree "acgttgaccagtcagtacggtgacacacattt"

testNumberOfNodes = numberOfNodes testTree
testNumberOfNodesLong = numberOfNodes testTree2

listOfLeaves = dfsLeaves testTree
listOfLeavesLong = dfsLeaves testTree2

main :: IO ()
main =
  do
    putStrLn "--------------------------------------------------------"
    putStrLn "Example Tree: "
    putStrLn ""
    putStrLn (show testTree)
    putStrLn ""
    putStrLn "--------------------------------------------------------"
    putStrLn "List the leaf labels of 'abcab'"
    putStrLn ""
    putStrLn (show listOfLeaves)
    putStrLn ""
    putStrLn "--------------------------------------------------------"
    putStrLn "List the leaf labels of 'acgttgaccagtcagtacggtgacacacattt'"
    putStrLn ""
    putStrLn (show listOfLeavesLong)
    putStrLn ""
    putStrLn "--------------------------------------------------------"
    putStrLn "Test number of nodes in 'abcab'"
    putStrLn (show testNumberOfNodes)
    putStrLn ""
    putStrLn "--------------------------------------------------------"
    putStrLn "Test number of nodes in 'acgttgaccagtcagtacggtgacacacattt'"
    putStrLn ""
    putStrLn (show testNumberOfNodesLong)

