module Chapter4.ContainersSpec where
    
import Test.Hspec
import Chapter4.Containers
import Data.Char
import Chapter2.DataTypes
import Chapter3.Stubs
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tree
import Data.Graph

spec :: Spec
spec = do
    describe "Prelude.head" $ do

        it "Insert in map" $ do
            (insertF "key2" "value2" (M.singleton "key1" "value1")) 
            `shouldBe` (M.fromList [("key1", "value1"), ("key2", "value2")])

        it "Delete in map" $ do
            (deleteF "key2" (M.fromList [("key1", "value1"), ("key2", "value2")]))
            `shouldBe` (M.singleton "key1" "value1")
            
        it "Adjust existing value in map" $ do
            (adjustF (\str -> map toUpper str) "key2" (M.fromList [("key1", "value1"), ("key2", "value2")]))
            `shouldBe` (M.fromList [("key1", "value1"), ("key2", "VALUE2")])

        it "Adjust missed value in map" $ do
            (adjustF (\str -> map toUpper str) "key3" (M.fromList [("key1", "value1"), ("key2", "value2")]))
            `shouldBe` (M.fromList [("key1", "value1"), ("key2", "value2")])

        it "Classify clients method A" $ do
            let expected = M.fromList [(GovOrgKind, S.fromList [GovOrg {clientId = 3, clientName = "NTTF"}]),
                                     (CompanyKind, S.fromList [Company {clientId = 4, 
                                                                     clientName = "Wormhole Inc.", 
                                                                     person = Person {firstName = "Karl", 
                                                                                      lastName = "Schwarzschild", 
                                                                                      gender = Male}, 
                                                                                      duty = "yes"}]),
                                     (IndividualKind, S.fromList [Individual {clientId = 2, 
                                                                           person = Person {firstName = "H.G.", 
                                                                                            lastName = "Wells", 
                                                                                            gender = Male}},
                                     Individual {clientId = 5, person = Person {firstName = "Doctor", 
                                                                                lastName ="", 
                                                                                gender = Male}},
                                     Individual {clientId = 6, person = Person {firstName = "Sarah", 
                                                                                lastName = "Jane", 
                                                                                gender = Female}}])]
             in (classifyClientsA listOfClients) `shouldBe` expected

        
        it "Classify clients method B" $ do
            (classifyClientsB listOfClients) `shouldBe` (classifyClientsA listOfClients)

        it "Traverse pre order" $ do
            let tree = Node 1 [ Node 2 [ Node 3 [],
                                         Node 4 [],
                                         Node 5 [] ]
                              , Node 6 [] ]
             in preOrder show tree `shouldBe` ["1", "2", "3", "4", "5", "6"]
             
        it "Check precedence" $ do
            let (g,v,_) = timeMachinePrecedence
             in (map (\x -> let (k,_,_) = v x in k) $ topSort g) 
             `shouldBe` ["wood","plastic","walls","aluminum","door","wheels","done"]

        it "Check travel" $ do
            (path timeMachineTravel 1302 917) `shouldBe` True



-- Helpers

timeMachineTravel :: Graph
timeMachineTravel = buildG (103,2013)
                [(1302,1614),(1614,1302),(1302,2013),(2013,1302),(1614,2013)
                ,(2013,1408),(1408,1993),(1408,917),(1993,917),(907,103),(103,917)]