import Control.Applicative
import Control.Monad

data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Show)

nodeFromIndex :: Int -> [(Int, Int)] -> BinTree Int
nodeFromIndex idx indexes = buildTree idx (indexes !! (idx-1)) indexes
  
buildTree :: Int -> (Int, Int) -> [(Int, Int)] -> BinTree Int
buildTree idx (-1, -1) indexes = Node idx Empty Empty
buildTree idx (ll, -1) indexes = Node idx (nodeFromIndex ll indexes) Empty
buildTree idx (-1, lr) indexes = Node idx Empty (nodeFromIndex lr indexes)
buildTree idx (ll, lr) indexes = Node idx (nodeFromIndex ll indexes) (nodeFromIndex lr indexes)

readTree :: IO (BinTree Int)
readTree = do
  n <- read <$> getLine :: IO Int
  
  indexes <- replicateM n $ do
    [a, b] <- (fmap (read :: String -> Int) . words) <$> getLine
    return (a, b)

  return $ nodeFromIndex 1 indexes

readOrders :: IO [Int]
readOrders = do
  n <- read <$> getLine :: IO Int
  replicateM n $ read <$> getLine

swapOperation :: Int -> BinTree Int -> BinTree Int
swapOperation k t = swapOperationRec k t 1
  where
    swapOperationRec :: Int -> BinTree Int -> Int -> BinTree Int
    swapOperationRec k (Node v left right) depth
      | depth `mod` k == 0 = Node v (swapOperationRec k right $ depth + 1) (swapOperationRec k left $ depth + 1)
      | otherwise          = Node v (swapOperationRec k left $ depth + 1) (swapOperationRec k right $ depth + 1)
    swapOperationRec k tree _ = tree

traverseInOrder :: (Show a) => BinTree a -> [a]
traverseInOrder Empty = []
traverseInOrder (Node v left right) = (traverseInOrder left) ++ [v] ++ (traverseInOrder right)

main = do
  tree <- readTree
  ks <- readOrders
  foldM (\curTree order -> do
             let newTree = swapOperation order curTree
             putStrLn $ tail . init . show $ traverseInOrder newTree
             return newTree
         ) tree ks
  return ()
