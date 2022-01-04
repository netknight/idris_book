a: (Int, String)
a = (94, "Pages")

b: Int
b = fst a

c: String
c = snd a

bc: String
bc = cast b ++ c

print_bc: IO ()
print_bc = putStrLn bc
