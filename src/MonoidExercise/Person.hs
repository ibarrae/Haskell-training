module MonoidExercise.Person where

data Person 
    = Person Age String
    deriving (Show, Eq)

newtype Age 
    = Age Int
    deriving (Show, Eq)

instance Monoid Person where
    mempty = Person (Age 0) ""
    mappend a b = Person (mappend (getPersonAge a) (getPersonAge b)) (mappend (getName a) (getName b))

instance Monoid Age where
    mempty = Age 0
    mappend a b = Age ((+) (getAge a) (getAge b))

getPersonAge :: Person -> Age
getPersonAge (Person a _) = a

getAge :: Age -> Int
getAge (Age a) = a

getName :: Person -> String
getName (Person _ n) = n
