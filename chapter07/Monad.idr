interface Functor f => Applicative' (f: Type -> Type) where
  pure: a -> f a
  (<*>): f (a -> b) -> f a -> f b

interface Applicative' m => Monad' (m: Type -> Type) where
  (>>=): m a -> (a -> m a) -> m b
  join: m (m a) -> m a
