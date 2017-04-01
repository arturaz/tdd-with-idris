import Data.Vect

total createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

total transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (row :: rows) = zipWith (::) row (transposeMat rows)

total addMatrix : Num a =>
  Vect n (Vect m a) ->
  Vect n (Vect m a) ->
  Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

total multMatrixCalculateCell : Num a => Vect n a -> Vect n a -> a
multMatrixCalculateCell row transposedColumn =
  sum (zipWith (*) row transposedColumn)

total multMatrixCalculateRow : Num a =>
  Vect m a -> Vect n (Vect m a) -> Vect n a
multMatrixCalculateRow row1 [] = []
multMatrixCalculateRow row1 (m2_row :: m2_rest) =
  let
    cell = multMatrixCalculateCell row1 m2_row
    other_cells = multMatrixCalculateRow row1 m2_rest
  in
    cell :: other_cells

total multMatrix : Num a =>
  Vect height_1 (Vect shared a) ->
  Vect shared (Vect width_2 a) ->
  Vect height_1 (Vect width_2 a)
multMatrix matrix1 matrix2 =
  let
    matrix2_transposed = transposeMat matrix2
  in
    map (\row => multMatrixCalculateRow row matrix2_transposed) matrix1
