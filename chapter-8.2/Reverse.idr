import Data.Vect

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) =
  proofed (myReverse xs ++ [x])
  where
    proofed : Vect (len + 1) elem -> Vect (S len) elem
    proofed {len} v = rewrite plusCommutative 1 len in v
