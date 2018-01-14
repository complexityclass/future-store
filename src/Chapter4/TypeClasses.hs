module Chapter4.TypeClasses (
    ) where

import Chapter2.DataTypes

class Nameable n where 
    name :: n -> String