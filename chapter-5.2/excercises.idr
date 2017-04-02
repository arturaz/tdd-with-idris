import Data.String
import System

readNat : IO (Maybe Nat)
readNat = map (parsePositive {a=Nat}) getLine

guessToAction : (target : Nat) -> (user_guess : Nat) -> Maybe String
guessToAction target user_guess =
  case compare user_guess target of
    LT => Just "Guess higher!"
    GT => Just "Guess lower!"
    EQ => Nothing

readGuess : (target : Nat) -> IO (Maybe String)
readGuess target =
  do
  Just user_guess <- readNat | Nothing => pure (Just "Not a number!")
  pure (guessToAction target user_guess)

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses =
  do
  putStr ("Guess a number (guess=" ++ (show guesses) ++ "): ")
  action <- readGuess target
  case action of
    Nothing => putStrLn "Congratulations, you've guessed right!"
    Just hint =>
      do putStrLn hint
         guess target (guesses + 1)

my_repl : (prompt : String) -> (onInput : String -> String) -> IO ()
my_repl prompt onInput =
  do
  putStr prompt
  input <- getLine
  putStr (onInput input)
  my_repl prompt onInput

my_repl_with :
  (state : a) -> (prompt : String) ->
  (onInput : a -> String -> Maybe (String, a)) -> IO ()
my_repl_with state prompt onInput =
  do
  putStr prompt
  input <- getLine
  case onInput state input of
    Nothing => pure ()
    Just (output, newState) =>
      do
      putStr output
      my_repl_with newState prompt onInput

main : IO ()
main =
  do
  rnd_number <- map (\x => fromIntegerNat ((mod x 100) + 1)) time
  guess rnd_number 1
