module Chapter3.Stubs(
    buildGovOrg,
    buildIndividual,
    buildClientWithName
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

