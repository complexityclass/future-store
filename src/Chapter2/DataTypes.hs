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
    getClientName,
    genderStat,
    performSale,
    unzip',
    ConnOptions(),
    connDefault,
    greet
    ) where

import Data.Char

data Client i = GovOrg   { clientId :: i, clientName :: String }
              | Company  { clientId :: i, clientName :: String, person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Eq)

data Person = Person { firstName :: String, lastName :: String, gender :: Gender }
              deriving (Show, Eq)

data Gender = Male | Female | Unknown
              deriving (Show, Eq)

data TimeMachine = TimeMachine { info :: TimeMachineInfo
                               , price :: Float } deriving (Show, Eq)

data Producer = Producer { name :: String } deriving (Show, Eq)

data TimeMachineInfo = TimeMachineInfo { producer :: Producer
                                       , model :: Int
                                       , new :: Bool } deriving (Show, Eq)  

getClientName :: Client a -> String
getClientName client = case client of
    GovOrg  _ name      -> name
    Company _ name _ _  -> name
    Individual _ (Person fName lName _) -> fName ++ " " ++ lName

companyName :: Client a -> Maybe String
companyName client = case client of
    Company _ name _ _  -> Just name
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

genderStat :: [Client a] -> GenderStatInfo
genderStat clients = 
    let calc [] stats = stats
        calc (client:xs) stats = 
            let applyStat (Individual _ (Person _ _ gender)) stat = countGender stat gender
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


responsibility :: Client a -> String
responsibility (Company _ r _ _ ) = r
responsibility _ = "Unknown"

specialClient :: Client a -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False 


-- Records
greet :: Client a -> String
greet Individual { person = Person { .. } } = "Hi, " ++ firstName
greet Company { .. } = "Hello, " ++ clientName
greet GovOrg { } = "Welcome"

nameInCapitals :: Person -> Person
nameInCapitals p@(Person { firstName = initial:rest }) =
    let newName = (toUpper initial):rest
     in p { firstName = newName }
nameInCapitals p@(Person { firstName = ""}) = p


-- Default values

data ConnType = TCP | UDP
data UserProxy = NoProxy | Proxy String
data Timeout = NoTimeOut | TimeOut Integer
data ConnOptions = ConnOptions { connType      :: ConnType
                               , connSpeed     :: Integer
                               , connProxy     :: UserProxy
                               , connCaching   :: Bool
                               , connKeepAlive :: Bool
                               , connTimeOut   :: Timeout }
data Connection = Connection

connDefault :: ConnOptions
connDefault = ConnOptions TCP 0 NoProxy False False NoTimeOut

connect' :: String -> ConnOptions -> Connection
connect' _ = undefined