module Data.Ring
    (Ring(..), ringTableAdd, ringTableMul)
    where

class Num n => Ring n where
    -- A Ring is an Abelian Group
    (<+>) :: n -> n -> n
    idAdd :: n
    (<->) :: n -> n
    -- A Ring is a Monoid under Multiplication
    (<*>) :: n -> n -> n
    idMul :: n

ringTableAdd :: (Ring a, Enum a, Monoid a, Num a) => [[a]]
ringTableAdd = map (\p->map (p +) [mempty ..]) [mempty ..]

ringTableMul :: (Ring a, Enum a, Monoid a, Num a) => [[a]]
ringTableMul = map (\p->map (p *) $ enumFrom mempty) $ enumFrom mempty

-- Misc Utils
ringMult n = map (\x->map (\y->(x*y)`mod`(n+1)) [0..n]) [0..n]
ringSqrs n = map (\ns->ns!!(ns!!1)) $ ringMult n
