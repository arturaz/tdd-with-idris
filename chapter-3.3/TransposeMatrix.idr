import Data.Vect

total createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

total transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) =
  let xsTrans = transposeMat xs in
  zipWith (::) x xsTrans

total addMatrix : Num a =>
  Vect n (Vect m a) ->
  Vect n (Vect m a) ->
  Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

foo : Num a => 
  (matrix1 : Vect height_1 (Vect shared a)) -> 
  (matrix2 : Vect width_2 (Vect shared a)) -> 
  Vect height_1 (Vect width_2 a)
foo [] [] = []
foo [] (x :: xs) = []
foo (x :: xs) [] = [] :: foo xs []
foo (x :: xs) (y :: ys) = (?foo_rhs_4 :: ?foo_rhs_5) :: foo xs (y :: ys)

total multMatrix : Num a =>
  Vect height_1 (Vect shared a) ->
  Vect shared (Vect width_2 a) ->
  Vect height_1 (Vect width_2 a)
multMatrix matrix1 matrix2 =
  let matrix2_transposed = transposeMat matrix2 in
    foo matrix1 matrix2_transposed
