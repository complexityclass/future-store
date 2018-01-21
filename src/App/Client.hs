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
import App.CommonInterfaces

data Client i = GovOrg   { clientId :: i, clientName :: String }
              | Company  { clientId :: i, clientName :: String, person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show)


data Person = Person { firstName :: String, lastName :: String, gender :: Gender }
              deriving (Show, Eq, Ord, Read)


data Gender = Male | Female | Unknown
              deriving (Show, Eq, Ord, Read)


data TimeMachine = TimeMachine { info :: TimeMachineInfo
                               , price :: Float } deriving (Show, Eq)


data Producer = Producer { producerName :: String } deriving (Show, Eq)


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


instance Nameable (Client i) where
    name Individual { person = Person { firstName = f, lastName = n } } = f ++ " " ++ n
    name c = clientName c

instance Priceable TimeMachine where
    price TimeMachine { info = _, price = p } = p

instance Eq i => Eq (Client i) where
    GovOrg   { clientId = lId, clientName = lName } == 
        GovOrg   { clientId = rId, clientName = rName } = (lId == rId) && (lName == rName)
    Individual { person = lp } == Individual { person = rp } = lp == rp
    Company  { clientId = lc, clientName = ln, person = lp, duty = ld} == 
        Company  { clientId = rc, clientName = rn, person = rp, duty = rd} 
        = (lc == rc) && (ln == rn) && (lp == rp) && (ld == rd)
    _ == _ = False

instance Eq i => Ord (Client i) where
    compare l r = 
        case (compare (name l) (name r)) of
            GT -> GT
            LT -> LT
            EQ -> compare_ l r
    
compare_ :: (Client i) -> (Client i) -> Ordering
compare_ (Individual {}) (GovOrg {}) = GT
compare_ (Individual {}) (Company {}) = GT
compare_ (GovOrg {}) (Individual {}) = LT
compare_ (Company {}) (Individual {}) = LT
compare_ (Company {}) (GovOrg {}) = GT
compare_ (GovOrg {}) (Company {}) = LT
compare_ (GovOrg {}) (GovOrg {}) = EQ
compare_ (Company {person = lp}) (Company {person = rp}) = compare lp rp
compare_ (Individual {}) (Individual {}) = EQ
compare_ _ _ = EQ
                    





