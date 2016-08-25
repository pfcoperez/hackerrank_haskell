import Control.Applicative
import Control.Monad
  
data BTree a = Empty | BTree a (BTree a) (BTree a)

addElement :: (Ord a) => a -> BTree a -> BTree a
addElement v Empty = BTree v Empty Empty
addElement nv (BTree v left right)
  | nv <= v   = BTree v (addElement nv left) right
  | otherwise = BTree v left (addElement nv right)

buildTree :: (Ord a) => [a] -> BTree a
buildTree l = foldl (flip addElement) Empty l

preOrder :: BTree a -> [a]
preOrder Empty = []
preOrder (BTree v left right) = [v] ++ (preOrder left) ++ (preOrder right)

wasBTree :: (Ord a) => [a] -> Bool
wasBTree l = l == (preOrder . buildTree $ l)

main = do
  nCases <- read <$> getLine
  replicateM nCases $ do
    n <- read <$> getLine :: IO Int
    sample <- (((map read) . words) <$> getLine) :: IO [Int]
    putStrLn $ if (wasBTree sample) then "YES" else "NO"
  
    
  

