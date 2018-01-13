{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TransformListComp #-}

module Chapter2.DataTypes (
    Client (..),
    Person (..),
    Gender (..),
    GenderStatInfo (..),
    TimeMachine (..),
    Producer (..),
    TimeMachineInfo (..),
    ClientKind (..),
    getClientName,
    genderStat,
    performSale,
    unzip',
    compareClient,
    ConnOptions(),
    connDefault,
    greet,
    classifyClientsA,
    classifyClientsB
    ) where

import Data.Char
import Data.List
import GHC.Exts
import qualified Data.Map as M
import qualified Data.Set as S


data Client i = GovOrg   { clientId :: i, clientName :: String }
              | Company  { clientId :: i, clientName :: String, person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Eq, Ord)

data Person = Person { firstName :: String, lastName :: String, gender :: Gender }
              deriving (Show, Eq, Ord)

data Gender = Male | Female | Unknown
              deriving (Show, Eq, Ord)

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

---
compareClient :: Client a -> Client a -> Ordering
compareClient (Individual { person = p1 }) (Individual { person = p2 }) 
    = compare (firstName p1) (firstName p2)

compareClient (Individual {}) _ = GT
compareClient _ (Individual {}) = LT
compareClient c1 c2 = compare (clientName c1) (clientName c2)

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

---

companyDutiesAnalytics :: [Client a] -> [String]
companyDutiesAnalytics = map (duty . head) .
                           sortBy (\x y -> compare (length y) (length x)) .
                           groupBy (\x y -> duty x == duty y) .
                           filter isCompany
                        where isCompany (Company {}) = True
                              isCompany _            = False


companyAnalytics :: [Client a] -> [(String, [(Person, String)])]
companyAnalytics clients = [ (the clientName, zip person duty) 
                           | client@(Company {..}) <- clients
                           , then sortWith by duty
                           , then group by clientName using groupWith
                           , then sortWith by length client]


-- Chapter 4

data ClientKind  = GovOrgKind | CompanyKind | IndividualKind deriving (Show, Eq, Ord)


classifyClientsA :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClientsA [] =  M.fromList [(GovOrgKind, S.empty), (CompanyKind, S.empty), (IndividualKind, S.empty)]
classifyClientsA (x:xs) = case x of
    GovOrg _ _      -> M.unionWith (S.union) (M.singleton GovOrgKind (S.singleton x)) (classifyClientsA xs)
    Company _ _ _ _ -> M.unionWith (S.union) (M.singleton CompanyKind (S.singleton x)) (classifyClientsA xs)  
    Individual _ _  -> M.unionWith (S.union) (M.singleton IndividualKind (S.singleton x)) (classifyClientsA xs)


classifyClientsB :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClientsB list = 
    let govOrgKind = filter (\x -> case x of GovOrg _ _ -> True
                                             _          -> False) list
        companyKind = filter (\x -> case x of Company _ _ _ _ -> True
                                              _           -> False) list
        individualKind = filter (\x -> case x of Individual _ _ -> True
                                                 _           -> False) list
     in M.fromList [(GovOrgKind, (S.fromList govOrgKind)), 
                    (CompanyKind, (S.fromList companyKind)), 
                    (IndividualKind, (S.fromList individualKind))]