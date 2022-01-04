data Dec_: (prop: Type) -> Type where
  Yes: (prf: prop) -> Dec_ prop
  No: (contra: prop -> Void) -> Dec_ prop

twoPlusTwoNotFive: 2 + 2 = 5 -> Void
twoPlusTwoNotFive Refl impossible

-- Not total example (could loop forever)
loop: Void
loop = loop

valueNotSuc: (x: Nat) -> x = S x -> Void
valueNotSuc _ Refl impossible
