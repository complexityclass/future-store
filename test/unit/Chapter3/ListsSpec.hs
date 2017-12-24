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