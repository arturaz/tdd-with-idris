import Data.Vect

data Format : Type where
  Number : (fmt : Format) -> Format
  Str : (fmt : Format) -> Format
  Character : (fmt : Format) -> Format
  Dbl : (fmt : Format) -> Format
  Literal : (literal : String) -> (fmt : Format) -> Format
  End : Format

-- builds the required type of printf function from given format
PrintfType : Format -> Type
PrintfType (Number fmt) = Int -> PrintfType fmt
PrintfType (Str fmt) = String -> PrintfType fmt
PrintfType (Character fmt) = Char -> PrintfType fmt
PrintfType (Dbl fmt) = Double -> PrintfType fmt
PrintfType (Literal literal fmt) = PrintfType fmt
PrintfType End = String

-- builds a function that turns parameters to formatted string
printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc = \s => printfFmt fmt (acc ++ s)
printfFmt (Character fmt) acc = \c => printfFmt fmt (acc ++ (strCons c ""))
printfFmt (Dbl fmt) acc = \d => printfFmt fmt (acc ++ show d)
printfFmt (Literal literal fmt) acc = printfFmt fmt (acc ++ literal)
printfFmt End acc = acc

-- parses char list into format instance
toFormatList : (xs : List Char) -> Format
toFormatList [] = End
toFormatList ('%' :: 'd' :: chars) = Number (toFormatList chars)
toFormatList ('%' :: 's' :: chars) = Str (toFormatList chars)
toFormatList ('%' :: 'c' :: chars) = Character (toFormatList chars)
toFormatList ('%' :: 'f' :: chars) = Dbl (toFormatList chars)
toFormatList (c :: chars) =
  case toFormatList chars of
    -- Don't string literals one after another, join them into one.
    Literal literal fmt => Literal (strCons c literal) fmt
    fmt => Literal (strCons c "") fmt

toFormat : String -> Format
toFormat s = toFormatList (unpack s)

printf : (fmt : String) -> PrintfType (toFormat fmt)
printf fmt = printfFmt _ ""

Matrix : Nat -> Nat -> Type
Matrix x y = Vect x (Vect y Double)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

TupleVect : Nat -> Type -> Type
TupleVect Z t = ()
TupleVect (S k) t = (t, TupleVect k t)

test : TupleVect 4 Nat
test = (1, 2, 3, 4, ())
