module Lib where


-- | Intention of Vote
type Intention  = Double

-- | Estimated abstention
type Abstention = Double

vote :: Intention -> Abstention -> (Abstention, Abstention) -> (Double, Double)
vote i a (a1, a2) = (a' / total, b' / total)
    where
        ia = i
        ib = 1 - i

        a' = ia * a1 * a
        b' = ib * a2 * a
        
        total = a' + b'



someFunc :: IO ()
someFunc = putStrLn "someFunc"
