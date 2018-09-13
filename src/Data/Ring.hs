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
ringTableMul = map (\p->map (p *) [mempty ..]) [mempty ..]
