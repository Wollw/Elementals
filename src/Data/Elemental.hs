module Data.Elemental
    (Elemental(..))
    where

import Data.Ring

import Data.Monoid
import Data.Group

data Elemental = Earth | Air | Water | Fire deriving (Show, Enum, Eq)

instance Ring Elemental where
    x <+> y = x + y
    x <> y = x * y
    (-) = negate

instance Num Elemental where
    fromInteger = toEnum . fromIntegral . flip mod 4
-- Monoid over Multiplication
    x * y = toEnum $ flip mod 4 $ getProduct
                (  (Product . fromEnum $ x)
                Data.Monoid.<> (Product . fromEnum $ y))
-- Abelian Group over Addition
    x + y = toEnum $ flip mod 4 $ getSum
                (  (Sum . fromEnum $ x)
                Data.Monoid.<> (Sum . fromEnum $ y))
    negate = invert
    abs x = x
    signum Earth = Earth
    signum x = Air

-- Abelian Group over Addition.
instance Monoid Elemental where
    mempty = Earth
    mappend = (+)
instance Group Elemental where
    invert Air = Fire
    invert Fire = Air
    invert x = x 
instance Abelian Elemental
