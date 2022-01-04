occurences: Eq ty => (item: ty) -> (values: List ty) -> Nat
occurences item [] = 0
occurences item (x :: xs) = case x == item of
                                 False => occurences item xs
                                 True => 1 + occurences item xs

data Matter = Solid | Liquid | Gas

Eq Matter where
  Solid == Solid = True
  Liquid == Liquid = True
  Gas == Gas = True
  _ == _ = False
  x /= y = not (x == y)
