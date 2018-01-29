module App.TravelGuide (

) where

data TravelGuide = TravelGuide { title :: String, authors :: [String], price :: Double }
                   deriving (Show, Eq, Ord)
