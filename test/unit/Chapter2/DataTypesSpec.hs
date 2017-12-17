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


        it "Sale Timemachines" $ do
            let timeMachine1 = (TimeMachine (TimeMachineInfo (Producer "Eureka") 6 True) 100)
                timeMachine2 = (TimeMachine (TimeMachineInfo (Producer "Eureka") 8 True) 200)
                timeMachine3 = (TimeMachine (TimeMachineInfo (Producer "Eureka") 9 True) 400)
                result = performSale [timeMachine1, timeMachine2, timeMachine3] 0.5
                prices = map (\machine -> case machine of
                                          (TimeMachine _ value) -> value) result

             in prices `shouldBe` [50, 100, 200]

        it "Unzip lists" $ do
            let input = [(1,2), (3,4), (5,6)]
             in unzip' input `shouldBe` ([1,3,5], [2,4,6])

        it "Greet clien " $ do
            let client = IndividualR { person = PersonR { lastName = "Smith", firstName = "John" } }
                greeting = greet client
             in greeting `shouldBe` "Hi, John"

