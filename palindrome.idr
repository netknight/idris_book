palindrome: String -> Bool
palindrome str = toLower str == reverse (toLower str)

palindromeWithLength: Nat -> String -> Bool
palindromeWithLength len str = if length str < len then False
                               else palindrome str

counts: String -> (Nat, Nat)
counts str = (wordCount str, length str)
 where
   wordCount: String -> Nat
   wordCount str = length (words str)

main: IO ()
main = repl "Enter a string: "
  (\str => show (palindrome str) ++ "\n")
