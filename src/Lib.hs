module Lib where


-- | Intention of Vote
type Intention  = Double

-- | Estimated abstention
type Abstention = Double


vote :: (Abstention, Abstention) -> Intention -> Double
vote (a1, a2) i = i'
    where
        ia = i
        ib = 1 - i

        ia' = ia * a1
        ib' = ib * a2
        ----------------
        total = ia' + ib'
        
        -- >
        i' = ia' / total
        -- >


someFunc :: IO ()
someFunc = putStrLn "someFunc"
