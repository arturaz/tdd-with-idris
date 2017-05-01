import Data.Vect

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = rewrite plusZeroRightNeutral m in Refl
myPlusCommutes (S k) m = rewrite (myPlusCommutes k m) in plusSuccRightSucc m k

myReverse : Vect n a -> Vect n a
myReverse xs =
  reverse' [] xs
  where
    reverse' : Vect n a -> Vect m a -> Vect (n + m) a
    reverse' acc [] =
      reverseProof_nil acc
      where
        reverseProof_nil : Vect n1 a -> Vect (n1 + 0) a
        reverseProof_nil {n1} xs = rewrite plusZeroRightNeutral n1 in xs
    reverse' acc (x :: xs) =
      reverseProof_xs (reverse' (x :: acc) xs)
      where
        reverseProof_xs : Vect (S (n1 + len)) a -> Vect (n1 + (S len)) a
        reverseProof_xs {n1} {len} xs =
          rewrite sym (plusSuccRightSucc n1 len) in xs
