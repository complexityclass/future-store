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
    genderStat,
    performSale,
    unzip',
    ConnOptions(),
    connDefault,
    greet,
    classifyClientsA,
    classifyClientsB,
    timeMachinePrecedence
    ) where

import Data.Char
import Data.List
import GHC.Exts
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Graph
import App.Client

-- Task 2.5
-- Write a function to return a count of persons groupped by gender
-- Write a function for sale time machines

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

-- Chapter 4

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


timeMachineGraph :: [(String, String, [String])]
timeMachineGraph = 
    [("wood","wood",["walls"]), ("plastic","plastic",["walls","wheels"])
    ,("aluminum","aluminum",["wheels","door"]),("walls","walls",["done"])
    ,("wheels","wheels",["done"]),("door","door",["done"]),("done","done",[])]

timeMachinePrecedence :: (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex)
timeMachinePrecedence = graphFromEdges timeMachineGraph