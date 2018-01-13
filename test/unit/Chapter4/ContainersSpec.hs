module Chapter4.ContainersSpec where
    
import Test.Hspec
import Chapter4.Containers
import Data.Char
import Chapter2.DataTypes
import Chapter3.Stubs
import qualified Data.Map as M
import qualified Data.Set as S

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