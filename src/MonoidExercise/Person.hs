module MonoidExercise.Person where

data Person 
    = Person Age String
    deriving (Show, Eq)

newtype Age 
    = Age Int
    deriving (Show, Eq)

instance Monoid Person where
    mempty = Person (Age 0) ""
    mappend (Person a1 n1) (Person a2 n2) = Person (a1 `mappend` a2) (n1 `mappend` n2)

instance Monoid Age where
    mempty = Age 0
    mappend (Age a1) (Age a2) = Age (a1 + a2)

getPersonAge :: Person -> Age
getPersonAge (Person a _) = a

getAge :: Age -> Int
getAge (Age a) = a

getName :: Person -> String
getName (Person _ n) = n
