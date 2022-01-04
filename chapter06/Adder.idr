AdderType: (numArgs: Nat) -> Type
AdderType Z = Int
AdderType (S k) = (next: Int) -> AdderType k

adder: (numArgs: Nat) -> (acc: Int) -> AdderType numArgs
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)

--

AdderTypeGeneric: (numArgs: Nat) -> (numType: Type) -> Type
AdderTypeGeneric Z numType = numType
AdderTypeGeneric (S k) numType = (next: numType) -> AdderTypeGeneric k numType

adderGeneric: Num numType => (numArgs: Nat) -> (acc: numType) -> AdderTypeGeneric numArgs numType
adderGeneric Z acc = acc
adderGeneric (S k) acc = \next => adderGeneric k (next + acc)
