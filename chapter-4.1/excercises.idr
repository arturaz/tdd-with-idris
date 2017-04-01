data Shape : Type where
  Triangle : (base : Double) -> (height : Double) -> Shape
  Rectangle : (length : Double) -> (height : Double) -> Shape
  Circle : (radius : Double) -> Shape

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture : Type where
  Primitive : (shape : Shape) -> Picture
  Combine : (pic1 : Picture) -> (pic2 : Picture) -> Picture
  Rotate : (angles : Double) -> (pic : Picture) -> Picture
  Translate : (x : Double) -> (y: Double) -> (pic : Picture) -> Picture

data Tree : Type -> Type where
  Empty : Ord a => Tree a
  Node : Ord a => (left: Tree a) -> (value: a) -> (right: Tree a) -> Tree a

insert : a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x orig @ (Node left value right) =
  case compare x value of
    LT => Node (insert x left) value right
    EQ => orig
    GT => Node left value (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left value right) =
  (treeToList left) ++ (value :: (treeToList right))

data Expr =
  Single Int
  | Add Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr

evaluate : Expr -> Int
evaluate (Single x) = x
evaluate (Add x y) = (evaluate x) + (evaluate y)
evaluate (Subtract x y) = (evaluate x) - (evaluate y)
evaluate (Multiply x y) = (evaluate x) * (evaluate y)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe x Nothing = x
maxMaybe (Just x) (Just y) = Just (if x > y then x else y)

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive t @ (Triangle base height)) = Just (area t)
biggestTriangle (Primitive shape) = Nothing
biggestTriangle (Combine pic1 pic2) = maxMaybe (biggestTriangle pic1) (biggestTriangle pic2)
biggestTriangle (Rotate angles pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))
