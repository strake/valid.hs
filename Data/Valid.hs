module Data.Valid where

data Valid e a = Failure e | Valid a
  deriving (Eq, Read, Show, Functor, Foldable, Traversable)

instance Semigroup e => Applicative (Valid e) where
    pure = Valid
    Failure x <*> Failure y = Failure (x <> y)
    Failure x <*> Valid   _ = Failure x
    Valid   _ <*> Failure y = Failure y
    Valid   f <*> Valid   x = Valid (f x)
