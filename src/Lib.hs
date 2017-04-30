module Lib where


-- | Intention of Vote
type Intention  = Double

-- | Estimated abstention
type Abstention = Double

vote :: Intention -> (Abstention, Abstention) -> (Double, Double)
vote i (a1, a2) = (a' / total, b' / total)
    where
        ia = i
        ib = 1 - i

        a' = ia * a1
        b' = ib * a2
        
        total = a' + b'



someFunc :: IO ()
someFunc = putStrLn "someFunc"
