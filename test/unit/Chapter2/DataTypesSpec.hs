module Chapter2.DataTypesSpec where
    
import Test.Hspec
import Chapter2.DataTypes
    
spec :: Spec
spec = do
    describe "Prelude.head" $ do
    
        it "Get client name" $ do
            clientName (Individual (Person "John" "Black" Male) True) `shouldBe` "John Black"

        it "Calculate gender stat" $ do
            let client1 = (Individual (Person "John" "Black" Male) True)
                client2 = (Individual (Person "Jack" "White" Male) True)
                client3 = (Individual (Person "Melissa" "Green" Female) True)
                client4 = GovOrg "Sony"
             in genderStat [client1, client2, client3, client4] `shouldBe` (GenderStatInfo 2 1)