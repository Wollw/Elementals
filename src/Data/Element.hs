module Data.Element
    (Element(..))
    where

import Data.Ring

import Data.Monoid
import Data.Group

data Element = Earth | Air | Water | Fire deriving (Show, Enum, Eq)

instance Ring Element where
    x <+> y = x + y
    x <*> y = x * y
    (<->) = negate

instance Num Element where
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
instance Monoid Element where
    mempty = Earth
    mappend = (+)
instance Group Element where
    invert Earth = Earth
    invert x = toEnum . (+4) . negate . fromEnum $ x
instance Abelian Element
