module Data.Planet
    (Planet(..))
    where

import Data.Monoid
import Data.Group
import Data.Ring

data Planet = Sun | Mercury | Venus | Moon | Mars | Jupiter | Saturn
    deriving (Show, Enum, Eq)

instance Ring Planet where
    x <+> y = x + y
    x <> y = x * y
    (-) = negate

instance Num Planet where
    fromInteger = toEnum . fromIntegral . flip mod 7
-- Monoid over Multiplication
    x * y = toEnum $ flip mod 7 $ getProduct
                (  (Product . fromEnum $ x)
                Data.Monoid.<> (Product . fromEnum $ y))
-- Abelian Group over Addition
    x + y = toEnum $ flip mod 7 $ getSum
                (  (Sum . fromEnum $ x)
                Data.Monoid.<> (Sum . fromEnum $ y))
    negate = invert
    abs x = x
    signum Sun = Sun
    signum x = Mercury

-- Abelian Group over Addition.
instance Monoid Planet where
    mempty = Sun
    mappend = (+)
instance Group Planet where
    invert = toEnum . (+7) . negate . fromEnum
instance Abelian Planet
