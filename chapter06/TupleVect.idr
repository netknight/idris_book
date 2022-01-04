TupleVect: (numArgs: Nat) -> (ty: Type) -> Type
TupleVect Z ty = ()
TupleVect (S k) ty = (ty, TupleVect k ty)

testVect: TupleVect 4 Nat
testVect = (1, 2, 3, 4, ())
