import Control.Applicative
import Control.Monad

-- (nrows, ncolumns)
type MatrixSize = (Int, Int)

type Matrix a = [[a]]

sizeMatrix :: Matrix a -> MatrixSize
sizeMatrix [] = (0, 0)
sizeMatrix (row:rows) = (1 + length rows, length row)

-- (size, border, kernel)
data RingMatrix a = EmptyRing | RingMatrix MatrixSize [a] (RingMatrix a) deriving Show

sizeRingMatrix :: RingMatrix a -> MatrixSize
sizeRingMatrix EmptyRing = (0,0)
sizeRingMatrix (RingMatrix size _ _) = size

crust :: Matrix a -> [a]
crust (firstRow:rs) = let innerRows = init rs
                          leftBorder = map head innerRows
                          rightBorder = map last innerRows
                          lastRow = last rs
                      in firstRow ++ rightBorder ++ (reverse lastRow) ++ (reverse leftBorder)

crustSize :: MatrixSize -> Int
crustSize (n,m) = 2*(m+n-2)

regular2ring :: Matrix a -> RingMatrix a
regular2ring  []  = EmptyRing
regular2ring [[]] = EmptyRing
regular2ring m = RingMatrix (nr, nc) (crust m) kernel
  where (nr,nc) = sizeMatrix m
        kernel = if (min nr nc) == 2 then
                   EmptyRing
                 else
                   let inner = init . tail in regular2ring . map inner $ inner m

flattenedRow :: Int -> RingMatrix a -> [a]
flattenedRow _ EmptyRing = []
flattenedRow r (RingMatrix (n,m) bark kernel)
  | r ==   0 && n > 0 = take m bark
  | r == n-1 && n == 2 = take m . reverse $ bark
  | r == n-1 = take m . drop (n-2) . reverse $ bark
  | otherwise = let barkLength = crustSize (n,m)
                    firstElem  = bark !! (barkLength-r)
                    lastElem   = bark !! (m-1+r)
                in [firstElem] ++ (flattenedRow (r-1) kernel) ++ [lastElem]

ring2regular :: RingMatrix a -> Matrix a
ring2regular rm@(RingMatrix (n,_) _ _) = map (\r -> flattenedRow r rm) [0..(n-1)]

rotateRing :: Int -> RingMatrix a -> RingMatrix a
rotateRing r (RingMatrix mSize mCrust mKernel) = RingMatrix mSize (rotate r mCrust) $ rotateRing r mKernel
  where rotate :: Int -> [a] -> [a]
        rotate n l = let (a,b) = splitAt (n `mod` (crustSize mSize)) l in b++a
rotateRing r EmptyRing = EmptyRing

rotateMatrix :: Int -> Matrix a -> Matrix a
rotateMatrix r = ring2regular . (rotateRing r) . regular2ring

readMatrix :: MatrixSize -> IO (Matrix Int)
readMatrix (n,m) = replicateM n $ (map (read :: String -> Int)) <$> words <$> getLine

main = do
  [[n,m,r]] <- readMatrix (1,3)
  m <- readMatrix (n,m)
  let res = rotateMatrix r m
  sequence $ map (putStrLn . (map (\c -> if c == ',' then ' ' else c)) . tail . init . show) res

--m = map (\x -> [x..x+3]) [10,20..40]
--m1 =
--  [
--    [1,	2,  3, 4],
--    [5,	6,  7, 8],
--    [9,	10,11,12],
--    [13,14,15,16]
--  ]
