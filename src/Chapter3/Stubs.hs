module Chapter3.Stubs(
    buildGovOrg,
    buildIndividual
) where

import Chapter2.DataTypes

buildPerson :: Person
buildPerson = Person "Jack" "Smith" Male 

buildGovOrg :: Client
buildGovOrg = GovOrg "Hospital"

buildIndividual :: Client
buildIndividual = Individual buildPerson True
