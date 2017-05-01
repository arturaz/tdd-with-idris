data Vect : (n : Nat) -> (a : Type) -> Type where
  Nil : Vect 0 a
  (::) : (x : a) -> (xs : Vect n a) -> Vect (S n) a

headUnequal : DecEq a =>
  {xs : Vect n a} -> {ys : Vect n a} ->
  (contra : (x = y) -> Void) ->
  ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a =>
  {xs : Vect n a} -> {ys : Vect n a} ->
  (contra : (xs = ys) -> Void) ->
  ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl

DecEq a => DecEq (Vect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) =
    case decEq x y of
      No headsUnequalContra => No (headUnequal headsUnequalContra)
      Yes Refl =>
        case decEq xs ys of
          No tailsNotEqualContra => No (tailUnequal tailsNotEqualContra)
          Yes Refl => Yes Refl
