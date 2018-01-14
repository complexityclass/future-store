{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TransformListComp #-}

module App.Client (
    Client (..),
    Person (..),
    Gender (..),
    GenderStatInfo (..),
    TimeMachine (..),
    Producer (..),
    TimeMachineInfo (..),
    ClientKind (..),
    getClientName,
    companyName,
    compareClient,

) where

import Data.Char
import Data.List
import GHC.Exts
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Graph

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


data GenderStatInfo = GenderStatInfo Int Int deriving (Show, Eq)


data ClientKind  = GovOrgKind | CompanyKind | IndividualKind deriving (Show, Eq, Ord)


getClientName :: Client a -> String
getClientName client = case client of
    GovOrg  _ name      -> name
    Company _ name _ _  -> name
    Individual _ (Person fName lName _) -> fName ++ " " ++ lName

companyName :: Client a -> Maybe String
companyName client = case client of
    Company _ name _ _  -> Just name
    _                   -> Nothing

compareClient :: Client a -> Client a -> Ordering
compareClient (Individual { person = p1 }) (Individual { person = p2 }) 
    = compare (firstName p1) (firstName p2)

compareClient (Individual {}) _ = GT
compareClient _ (Individual {}) = LT
compareClient c1 c2 = compare (clientName c1) (clientName c2)

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

