data Expr num =
  Val num
  | Add (Expr num) (Expr num)
  | Sub (Expr num) (Expr num)
  | Mul (Expr num) (Expr num)
  | Div (Expr num) (Expr num)
  | Abs (Expr num)

Num a => Num (Expr a) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

Neg a => Neg (Expr a) where
  negate x = 0 - x
  (-) = Sub
  abs = Abs

eval : (Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

Show num => Show (Expr num) where
  show (Val x) = show x
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Abs x) = "|" ++ show x ++ "|"

(Neg num, Integral num, Eq num) => Eq (Expr num) where
  (==) x y = (eval x) == (eval y)

(Neg num, Integral num) => Cast (Expr num) num where
  cast orig = eval orig

--------------------------------------------------------------------------------
-- 7.3 code

Functor Expr where
  map func (Val x) = Val (func x)
  map func (Add x y) = Add (map func x) (map func y)
  map func (Sub x y) = Sub (map func x) (map func y)
  map func (Mul x y) = Mul (map func x) (map func y)
  map func (Div x y) = Div (map func x) (map func y)
  map func (Abs x) = Abs (map func x)

data Vect : (len : Nat) -> (elem : Type) -> Type where
  ||| Empty vector
  Nil : Vect Z elem
  ||| A non-empty vector of length `S len`, consisting of a head element and
  ||| the rest of the list, of length `len`.
  (::) : (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem

Eq elem => Eq (Vect n elem) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = x == y && xs == ys

Foldable (Vect n) where
  foldr func init [] = init
  foldr func init (x :: xs) = func x (foldr func init xs)

  foldl func init [] = init
  foldl func init (x :: xs) = foldl func (func init x) xs 
