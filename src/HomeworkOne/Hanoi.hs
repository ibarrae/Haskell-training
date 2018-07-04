module HomeworkOne.Hanoi (getMovements, getMovementsWithExtraPeg) where
    
type Peg = String
type Move = (Peg, Peg)
getMovements :: Integer -> Peg -> Peg -> Peg -> [Move]
getMovements numberOfDiscs origin aux destiny
    | numberOfDiscs == 0 = []
    | numberOfDiscs == 1 = [(origin, destiny)]
    | otherwise = getMovements remainingDiscs origin destiny aux ++ [(origin, destiny)] ++ getMovements remainingDiscs aux origin destiny
    where remainingDiscs = numberOfDiscs - 1

getMovementsWithExtraPeg :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
getMovementsWithExtraPeg numberOfDiscs origin firstAux secondAux destiny
    | numberOfDiscs == 0 = []
    | numberOfDiscs == 1 = [(origin, destiny)]
    | otherwise = getMovementsWithExtraPeg remainingDiscs origin secondAux destiny firstAux ++ [(origin, secondAux)] ++ [(origin, destiny)] ++ [(secondAux, destiny)] ++ getMovementsWithExtraPeg remainingDiscs firstAux origin secondAux destiny
    where remainingDiscs = numberOfDiscs - 2