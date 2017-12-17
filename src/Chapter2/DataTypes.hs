{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Chapter2.DataTypes (
    Client (..),
    Person (..),
    Gender (..),
    GenderStatInfo (..),
    TimeMachine (..),
    Producer (..),
    TimeMachineInfo (..),
    clientName,
    genderStat,
    performSale,
    unzip',
    ClientR (..),
    PersonR (..),
    greet
    ) where

import Data.Char

data Client = GovOrg     String
            | Company    String Integer Person Bool
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show

data TimeMachine = TimeMachine { info :: TimeMachineInfo
                               , price :: Float } deriving (Show, Eq)

data Producer = Producer { name :: String } deriving (Show, Eq)

data TimeMachineInfo = TimeMachineInfo { producer :: Producer
                                       , model :: Int
                                       , new :: Bool } deriving (Show, Eq)  

clientName :: Client -> String
clientName client = case client of
    GovOrg name -> name
    Company name _ _ _ -> name
    Individual (Person fName lName _) _ -> fName ++ " " ++ lName

companyName :: Client -> Maybe String
companyName client = case client of
    Company name _ _ _  -> Just name
    _                   -> Nothing

-- Task 2.5
-- Write a function to return a count of persons groupped by gender
-- Write a function for sale time machines

data GenderStatInfo = GenderStatInfo Int Int deriving (Show, Eq)

maleCount :: GenderStatInfo -> Int
maleCount (GenderStatInfo male _) = male

femaleCount :: GenderStatInfo -> Int
femaleCount (GenderStatInfo _ female) = female

countGender :: GenderStatInfo -> Gender -> GenderStatInfo
countGender stat Male   = GenderStatInfo ((maleCount stat) + 1) (femaleCount stat)
countGender stat Female = GenderStatInfo (maleCount stat) ((femaleCount stat) + 1)
countGender stat Unknown = stat

genderStat :: [Client] -> GenderStatInfo
genderStat clients = 
    let calc [] stats = stats
        calc (client:xs) stats = 
            let applyStat (Individual (Person _ _ gender) _) stat = countGender stat gender
                applyStat _ stat                                  = stat
            in calc xs (applyStat client stats)
    in calc clients (GenderStatInfo 0 0)


performSale :: [TimeMachine] -> Float -> [TimeMachine]
performSale [] _ = []

-- Perform sale impl
-- performSale (x:xs) sale = [case x of
--                       (TimeMachine info gross) -> (TimeMachine info (gross * sale))] 
--                                                     ++ (performSale xs sale)

-- Task 2.7 Perform sale with record syntax
performSale (machine@(TimeMachine { .. }):xs) sale = 
    let currentPrice = price
     in [machine { price = currentPrice * sale }] ++ (performSale xs sale)                                              

-- Task 2.6
-- Write a function to unzip list of tuples                                                    

plus :: (Int, Int) -> ([Int], [Int]) -> ([Int], [Int])
plus (x, y) (xs, ys) = ([x] ++ xs, [y] ++ ys)                                                      

unzip' :: [(Int, Int)] -> ([Int], [Int])
unzip' [] = ([], [])
unzip' (x:xs) = plus ((fst x), (snd x)) (unzip' xs)


responsibility :: Client -> String
responsibility (Company r _ _ _ ) = r
responsibility _ = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False 


-- Records

data ClientR = GovOrgR  { clientRName :: String } 
             | CompanyR { clientRName :: String
                        , companyId :: Integer
                        , person :: PersonR
                        , duty :: String }
             | IndividualR { person :: PersonR }
             deriving Show

data PersonR = PersonR { firstName :: String
                       , lastName :: String }
                       deriving Show

greet :: ClientR -> String
greet IndividualR { person = PersonR { .. } } = "Hi, " ++ firstName
greet CompanyR { .. } = "Hello, " ++ clientRName
greet GovOrgR { } = "Welcome"

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR { firstName = initial:rest }) =
    let newName = (toUpper initial):rest
     in p { firstName = newName }
nameInCapitals p@(PersonR { firstName = ""}) = p