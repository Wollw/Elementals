module Data.Ring
    (Ring(..), ringTableAdd, ringTableMul)
    where

class Num n => Ring n where
    (<+>) :: n -> n -> n
    (<>) :: n -> n -> n
    (-) :: n -> n

ringTableAdd :: (Enum a, Monoid a, Num a) => [[a]]
ringTableAdd = map (\p->map (p +) [mempty ..]) [mempty ..]

ringTableMul :: (Enum a, Monoid a, Num a) => [[a]]
ringTableMul = map (\p->map (p *) [mempty ..]) [mempty ..]
