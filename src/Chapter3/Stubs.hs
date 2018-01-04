module Chapter3.Stubs(
    buildGovOrg,
    buildIndividual,
    buildClientWithName,
    listOfClients
) where

import Chapter2.DataTypes

buildPerson :: Person
buildPerson = Person "Jack" "Smith" Male 

buildGovOrg :: Client Int
buildGovOrg = GovOrg 123 "Hospital"

buildIndividual :: Client Int
buildIndividual = Individual 123 buildPerson

buildClientWithName :: String -> Client Int
buildClientWithName name = GovOrg 123 name

listOfClients = [ Individual 2 (Person "H.G." "Wells" Male)
                , GovOrg 3 "NTTF" -- National Time Travel Foundation
                , Company 4 "Wormhole Inc." (Person "Karl" "Schwarzschild" Male) "yes"
                , Individual 5 (Person "Doctor" "" Male)
                , Individual 6 (Person "Sarah" "Jane" Female)]