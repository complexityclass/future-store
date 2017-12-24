{-# LANGUAGE LambdaCase #-}

module Chapter3.Lists (
    maybeString,
    filterGovOrgs
) where

import Chapter2.DataTypes

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

filterGovOrgs :: [Client] -> [Client]
filterGovOrgs clients = filter (\case GovOrg _ -> True
                                      _        -> False) clients
