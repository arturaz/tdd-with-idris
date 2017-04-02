read_str_length : IO Nat
read_str_length = map Strings.length getLine

printLonger : IO ()
printLonger =
  do
  putStr "First string: "
  str1_length <- read_str_length
  putStr "Second string: "
  str2_length <- read_str_length
  printLn (max str1_length str2_length)

printLongerBind : IO ()
printLongerBind =
  putStr "First string: " >>= (\_ =>
    read_str_length >>= (\str1_length =>
      putStr "Second string: " >>= (\_ =>
        read_str_length >>= (\str2_length =>
          printLn (max str1_length str2_length)
        )
      )
    )
  )
