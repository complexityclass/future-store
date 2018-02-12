module App.TravelGuide (

) where

data TravelGuide = TravelGuide { title :: String, authors :: [String], price :: Double }
                   deriving (Show, Eq, Ord)

newtype TravelGuidePrice = TravelGuidePrice TravelGuide deriving Eq

instance Ord TravelGuidePrice where
    (TravelGuidePrice (TravelGuide t1 a1 p1)) <= (TravelGuidePrice (TravelGuide t2 a2 p2)) = 
        p1 < p2 || (p1 == p2 && (t1 < t2 || (t1 == t2 && a1 <= a2)))
