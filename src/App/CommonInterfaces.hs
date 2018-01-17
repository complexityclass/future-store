module App.CommonInterfaces (
    Nameable(..),
    Priceable(..),
    totalPrice
) where

class Nameable n where
    name :: n -> String


class Priceable n where 
    price :: n -> Float


totalPrice :: Priceable p => [p] -> Float
totalPrice = foldr (\x y -> (price x) + y) 0.0
