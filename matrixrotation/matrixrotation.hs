-- (nrows, ncolumns)
type MatrixSize = (Int, Int)

type Matrix a = [[a]]

sizeMatrix :: Matrix a -> MatrixSize
sizeMatrix [] = (0, 0)
sizeMatrix (row:rows) = (1 + length rows, length row)

-- (size, border, kernel)
data RingMatrix a = EmptyRing | RingMatrix MatrixSize [a] (RingMatrix a)

sizeRingMatrix :: RingMatrix a -> MatrixSize
sizeRingMatrix EmptyRing = (0,0)
sizeRingMatrix (RingMatrix size _ _) = size

regular2ring :: Matrix a -> RingMatrix a

