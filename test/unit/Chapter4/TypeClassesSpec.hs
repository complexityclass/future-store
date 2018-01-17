module Chapter4.TypeClassesSpec where

import Test.Hspec
import Chapter2.DataTypes
import App.CommonInterfaces
import App.Client

spec :: Spec
spec = do
    describe "Prelude.head" $ do
        
        it "Total price count" $ do
            let timeMachine1 = (TimeMachine (TimeMachineInfo (Producer "Eureka") 6 True) 100)
                timeMachine2 = (TimeMachine (TimeMachineInfo (Producer "Eureka") 8 True) 200)
                timeMachine3 = (TimeMachine (TimeMachineInfo (Producer "Eureka") 9 True) 400)
             in (totalPrice [timeMachine1, timeMachine2, timeMachine3]) `shouldBe` 700