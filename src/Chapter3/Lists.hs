{-# LANGUAGE LambdaCase #-}

module Chapter3.Lists (
    maybeString,
    filterGovOrgs,
    productP,
    productFold,
    minimumClient,
    minimumClient'
) where

import App.Client
import Chapter2.DataTypes
import Data.List hiding (head, tail)

-- maybeString :: Maybe t -> String
maybeString (Just _) = "Just"
maybeString Nothing  =  "Nothing"


data SamePair a = SamePair a a

sayHello :: [String] -> [String]
sayHello names = map (\name -> case name of
    "Alejandro" -> "Hello, writer"
    _           -> "Welcome, " ++ name
    ) names 

-- Filters

filterOnes :: [Int] -> [Int]
filterOnes = filter (== 1)

filterANumber :: Int -> [Int] -> [Int]
filterANumber n arr = filter (== n) arr

filterNot :: Int -> [Int] -> [Int]
filterNot n arr = filter (/= n) arr

filterGovOrgs :: [Client a] -> [Client a]
filterGovOrgs clients = filter (\case GovOrg _ _ -> True
                                      _        -> False) clients

permutationsStartingWith :: Char -> String -> [String]
permutationsStartingWith letter = filter (\l -> head l == letter) . permutations

---

mfoldr :: (a -> b -> b) -> b -> [a] -> b
mfoldr f initial [] = initial
mfoldr f initial (x:xs) = f x (mfoldr f initial xs)

data InfNumber a = MinusInfinity
                 | Number a
                 | PlusInfinity
                 deriving Show

infMax MinusInfinity x = x
infMax x MinusInfinity = x
infMax PlusInfinity _ = PlusInfinity
infMax _ PlusInfinity = PlusInfinity
infMax (Number a) (Number b) = Number (max a b)

maxNumber list = foldr (\x y -> infMax (Number x) y) MinusInfinity list


----

mfoldl :: (a -> b -> a) -> a -> [b] -> a
mfoldl _ initial [] = initial
mfoldl f initial (x:xs) = foldl f (f initial x) xs

--- Task 3.3

productP :: [Int] -> Int
productP [] = 1
productP (x:xs) = x * (productP xs)

productFold :: [Int] -> Int
productFold = foldr (*) 1 

minimumClient :: [Client a] -> Maybe (Client a)
minimumClient [] = Nothing
minimumClient clients = 
    let iter = \list acc -> case list of 
                             (x:[]) -> Just acc
                             (x:xs) -> let nameLength = length . getClientName
                                        in if nameLength x < nameLength acc 
                                            then iter xs x
                                            else iter xs acc 
     in iter clients (clients !! 1)

minimumClient' :: [Client a] -> Maybe (Client a)
minimumClient' clients = foldr (\client acc -> 
                                     let nameLength = length . getClientName
                                      in case acc of
                                        Nothing   -> Just client
                                        Just curr -> if nameLength curr > nameLength client
                                                      then Just client
                                                      else Just curr) Nothing clients
                                                      
skipUntilGov :: [Client a] -> [Client a]
skipUntilGov = dropWhile (\case { GovOrg {} -> False; _ -> True })

isIndividual :: Client a -> Bool
isIndividual (Individual {}) = True
isIndividual _               = False

checkIndividualAnalytics :: [Client a] -> (Bool, Bool)
checkIndividualAnalytics cs = (any isIndividual cs, not $ all isIndividual cs)

 

