module Data.Prima
    (Prima(..))
    where

import Data.Ring

import Data.Monoid
import Data.Group

data Prima = Quicksilver | Salt | Sulfur  deriving (Show, Enum, Eq)

instance Ring Prima where
    x <+> y = x + y
    x <*> y = x * y
    (<->) = negate
    idAdd = Quicksilver
    idMul = Salt

instance Num Prima where
    fromInteger = toEnum . fromIntegral . flip mod 3
-- Monoid over Multiplication
    x * y = toEnum $ flip mod 3 $ getProduct
                (  (Product . fromEnum $ x)
                Data.Monoid.<> (Product . fromEnum $ y))
-- Abelian Group over Addition
    x + y = toEnum $ flip mod 3 $ getSum
                (  (Sum . fromEnum $ x)
                Data.Monoid.<> (Sum . fromEnum $ y))
    negate = invert
    abs x = x
    signum Quicksilver = Quicksilver
    signum x = Salt

-- Abelian Group over Addition.
instance Monoid Prima where
    mempty = Quicksilver
    mappend = (+)
instance Group Prima where
    invert Quicksilver = Quicksilver
    invert x = toEnum . (+3) . negate . fromEnum $ x
instance Abelian Prima
