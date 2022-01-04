data DivResult = DivByZero | Result Double

safeDiv_: Double -> Double -> DivResult
safeDiv_ x y = if y == 0 then DivByZero
                        else Result (x / y)

safeDiv: Double -> Double -> Maybe Double
safeDiv x y = if y == 0 then Nothing
                        else Just (x / y)
