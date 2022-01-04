data Expr =
  Val Int
  |
  Add Expr Expr
  |
  Sub Expr Expr
  |
  Mult Expr Expr

%name Expr a, b

evaluate: Expr -> Int
evaluate (Val x) = x
evaluate (Add a b) = evaluate a + evaluate b
evaluate (Sub a b) = evaluate a - evaluate b
evaluate (Mult a b) = evaluate a * evaluate b
