interface Functor' (f: Type -> Type) where
  fmap: (func: a -> b) -> f a -> f b

Functor' List where
  fmap f [] = []
  fmap f (x :: xs) = f x :: fmap f xs
