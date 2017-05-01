import Data.Vect

append : Vect n elem -> Vect m elem -> Vect (m + n) elem
append {m} [] ys = rewrite plusZeroRightNeutral m in ys
append (x :: xs) ys =
  proof_xs (x :: append xs ys)
  where
    proof_xs : Vect (S (m + len)) elem -> Vect (plus m (S len)) elem
    proof_xs {m} {len} vect = rewrite sym (plusSuccRightSucc m len) in vect
