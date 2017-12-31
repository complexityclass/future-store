module Chapter3.ListsSpec where
    
import Test.Hspec
import Chapter3.Lists
import Chapter3.Stubs

spec :: Spec
spec = do
    describe "Prelude.head" $ do

        it "Maybe string" $ do
            (maybeString (Just "Some")) `shouldBe` "Just"

        it "Filter gov orgs" $ do
            let govOrg     = buildGovOrg
                individual = buildIndividual
             in (filterGovOrgs [govOrg, individual]) `shouldBe` [govOrg]

        it "Test products" $ do
            let sample = [1, 2, 3, 4, 5]
                res1 = productP sample
                res2 = productFold sample
             in [res1, res2] `shouldBe` [120, 120]

        it "Test min client name" $ do
            let a = buildGovOrg
                b = buildIndividual
                c = buildClientWithName "ABC"
                d = buildClientWithName "ABCDE"
             in minimumClient [a, b, c, d] `shouldBe` (Just c)


    