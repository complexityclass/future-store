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
    unzip'
    ) where

data Client = GovOrg     String
            | Company    String Integer Person Bool
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show

data TimeMachine = TimeMachine TimeMachineInfo Float deriving (Show, Eq)

data Producer = Producer String deriving (Show, Eq)

data TimeMachineInfo = TimeMachineInfo Producer Int Bool deriving (Show, Eq)  

clientName :: Client -> String
clientName client = case client of
    GovOrg name -> name
    Company name _ _ _ -> name
    Individual (Person fName lName _) _ -> fName ++ " " ++ lName

companyName :: Client -> Maybe String
companyName client = case client of
    Company name _ _ _  -> Just name
    _                   -> Nothing


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
performSale (x:xs) sale = [case x of
                      (TimeMachine info gross) -> (TimeMachine info (gross * sale))] 
                                                    ++ (performSale xs sale)


plus :: (Int, Int) -> ([Int], [Int]) -> ([Int], [Int])
plus (x, y) (xs, ys) = ([x] ++ xs, [y] ++ ys)                                                      

unzip' :: [(Int, Int)] -> ([Int], [Int])
unzip' [] = ([], [])
unzip' (x:xs) = plus ((fst x), (snd x)) (unzip xs)
 
