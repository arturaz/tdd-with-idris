same_cons : {list1 : List a} -> {list2 : List a} -> list1 = list2 -> x :: list1 = x :: list2
same_cons prf = cong prf

same_lists :
  {xs : List a} -> {ys: List a} ->
  x = y ->
  xs = ys ->
  x :: xs = y :: ys
same_lists Refl xsys_prf = cong xsys_prf

-- excercise 3

data ThreeEq : a -> b -> c -> Type where
  Same : ThreeEq x x x

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z Same = Same
