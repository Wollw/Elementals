module Data.Ring
    (Ring(..))
    where

class Num n => Ring n where
    (<+>) :: n -> n -> n
    (<>) :: n -> n -> n
    (-) :: n -> n
