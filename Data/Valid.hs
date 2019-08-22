module Data.Valid where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Classes
import Util (compose2)

data Valid e a = Failure e | Valid a
  deriving (Eq, Read, Show, Functor, Foldable, Traversable)

instance Semigroup e => Applicative (Valid e) where
    pure = Valid
    Failure x <*> Failure y = Failure (x <> y)
    Failure x <*> Valid   _ = Failure x
    Valid   _ <*> Failure y = Failure y
    Valid   f <*> Valid   x = Valid (f x)

instance Bifunctor Valid where bimap = bimapDefault
instance Bifoldable Valid where bifoldMap = bifoldMapDefault
instance Bitraversable Valid where bitraverse f g = fmap fromEither . bitraverse f g . toEither

instance Eq2 Valid where liftEq2 f g = compose2 (liftEq2 f g) toEither toEither

fromEither :: Either e a -> Valid e a
fromEither = either Failure Valid

toEither :: Valid e a -> Either e a
toEither (Failure e) = Left  e
toEither (Valid   a) = Right a
