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
multMatrixCalculateRow row1 matrix2 =
  let
    row1_r = replicate (length matrix2) row1
  in
    zipWith multMatrixCalculateCell row1_r matrix2

foo : Num a =>
  (matrix1 : Vect height_1 (Vect shared a)) ->
  (matrix2 : Vect width_2 (Vect shared a)) ->
  Vect height_1 (Vect width_2 a)
foo [] _ = []
foo (row1 :: matrix1Rest) matrix2 = ?foo1
  --let row1_replicated = replicate (length matrix2) row1
  --in multMatrixCalculateCell row1_replicated

total multMatrix : Num a =>
  Vect height_1 (Vect shared a) ->
  Vect shared (Vect width_2 a) ->
  Vect height_1 (Vect width_2 a)
multMatrix matrix1 matrix2 =
  let matrix2_transposed = transposeMat matrix2 in
    foo matrix1 matrix2_transposed
