StringOrInt: Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt: Bool) -> StringOrInt isInt
getStringOrInt False = "Nighty Four"
getStringOrInt True = 94

valToString: (isInt: Bool) -> StringOrInt isInt -> String
valToString False x = trim x
valToString True x = cast x

valToStr2: (isInt: Bool) -> (case isInt of
                                  False => String
                                  True => Int) -> String
valToStr2 False x = trim x
valToStr2 True x = cast x
