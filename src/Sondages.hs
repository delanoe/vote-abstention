module Sondages where

import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Map as M
import Data.Time.Segment
import Statistics.LinearRegression (linearRegression)
import qualified Data.Vector.Unboxed as U

data Candidat = Macron | LePen | Clinton | Trump
    deriving (Show, Read, Eq, Ord, Enum, Bounded)


data VoteIntention = VoteIntention { unCandidat :: Candidat
                                   , unIntention :: Double
             } deriving (Show, Read, Eq)

data Sondage = Sondage { unSondeur   :: T.Text
                       , unDateBegin :: UTCTime
                       , unDateEnd   :: Maybe UTCTime
                       , echantillon :: Maybe Int
                       , margeErreur :: Maybe Double
                       , abstention  :: Maybe Double
                       , indecis     :: Maybe Double
                       , intentions  :: [(Candidat, Double)]
                       } deriving (Show, Read, Eq)


sondages :: [Sondage]
sondages = [ Sondage "Harris"     (jour 2017 04 23) Nothing                  (Just 2684)   Nothing   Nothing     Nothing            [(Macron, 0.64)]
           , Sondage "Ipsos"      (jour 2017 04 23) Nothing                  (Just 1379)   Nothing   Nothing     Nothing            [(Macron, 0.62)]
           , Sondage "OpinionWay" (jour 2017 04 23) (Just $ jour 2017 04 24) (Just 1461)   Nothing   Nothing     Nothing     [(Macron, 0.61)]
           , Sondage "Ifop"       (jour 2017 04 23) (Just $ jour 2017 04 24) (Just 846)    Nothing  (Just 0.26) (Just 0.12)  [(Macron, 0.60)]
           , Sondage "Harris"     (jour 2017 04 25) (Just $ jour 2017 04 27) (Just 1016)   Nothing   Nothing     Nothing     [(Macron, 0.61)]
           , Sondage "OpinionWay" (jour 2017 04 25) (Just $ jour 2017 04 27) (Just 1500)   Nothing  (Just 0.25)  Nothing     [(Macron, 0.60)]
           , Sondage "Odoxa"      (jour 2017 04 26) (Just $ jour 2017 04 27) (Just 1003)   Nothing  (Just 0.20)   Nothing     [(Macron, 0.59)]
           , Sondage "BVA"        (jour 2017 04 26) (Just $ jour 2017 04 28) (Just 1506)   Nothing  (Just 0.245) (Just 0.15) [(Macron, 0.59)]
           , Sondage "Ipsos"      (jour 2017 04 28) (Just $ jour 2017 04 29) (Just 1504)   Nothing  (Just 0.25)   Nothing     [(Macron, 0.60)]
           , Sondage "Kantar"     (jour 2017 04 28) (Just $ jour 2017 04 30) (Just 1539)   Nothing   Nothing      Nothing     [(Macron, 0.59)]
           , Sondage "Elabe"      (jour 2017 04 28) (Just $ jour 2017 05 02) (Just 3956)   Nothing   Nothing     (Just 0.11)  [(Macron, 0.59)]
           , Sondage "Ifop"       (jour 2017 04 28) (Just $ jour 2017 05 02) (Just 1388)   Nothing  (Just 0.27)   Nothing     [(Macron, 0.595)]
           , Sondage "Cevifop"    (jour 2017 04 30) (Just $ jour 2017 05 01) (Just 13742)  Nothing  (Just 0.24)  (Just 0.15)  [(Macron, 0.59)]
           , Sondage "BVA"        (jour 2017 05 01) (Just $ jour 2017 05 02) (Just 1435)   Nothing  (Just 0.22)   Nothing     [(Macron, 0.60)]
           ]

polls :: [Sondage]
polls = [ Sondage "YouGov" (jour 2016 10 22) (Just $ jour 2016 10 26) (Just 1376) (Just 0.031) Nothing Nothing [(Clinton, 0.49), (Trump, 0.46)]
        , Sondage "ABC"    (jour 2016 10 23) (Just $ jour 2016 10 26) (Just 1150) (Just 0.03)  Nothing Nothing [(Clinton, 0.50), (Trump, 0.45)]
        , Sondage "USC"    (jour 2016 10 21) (Just $ jour 2016 10 27) (Just 3248) (Just 0.045) Nothing Nothing [(Clinton, 0.44), (Trump, 0.46)]
        , Sondage "Ipsos"  (jour 2016 10 21) (Just $ jour 2016 10 27) (Just 1627) (Just 0.03)  Nothing Nothing [(Clinton, 0.42), (Trump, 0.36)]
        , Sondage "IBD"    (jour 2016 10 22) (Just $ jour 2016 10 27) (Just 973)  (Just 0.033) Nothing Nothing [(Clinton, 0.45), (Trump, 0.42)]
        , Sondage "ABC"    (jour 2016 10 24) (Just $ jour 2016 10 27) (Just 1148) (Just 0.03)  Nothing Nothing [(Clinton, 0.49), (Trump, 0.46)]
        , Sondage "IBD"    (jour 2016 10 23) (Just $ jour 2016 10 28) (Just 1013) (Just 0.033) Nothing Nothing [(Clinton, 0.46), (Trump, 0.41)]
        , Sondage "ABC"    (jour 2016 10 25) (Just $ jour 2016 10 28) (Just 1160) (Just 0.03)  Nothing Nothing [(Clinton, 0.46), (Trump, 0.45)]
        ]

pollsResult :: [Sondage]
pollsResult = [Sondage "REAL" (jour 2016 11 08) Nothing Nothing Nothing Nothing Nothing [(Clinton, 0.481), (Trump, 0.46)]]

normalizePolls :: [Sondage] -> [Sondage]
normalizePolls xs = Prelude.map normalizePolls' xs
    where
        normalizePolls' (Sondage a b c d e f g is) = Sondage a b c d e f g is'
            where
                is' = Prelude.map (\(m, n) -> (m, n/total)) is
                total = sum $ Prelude.map snd is

sondage2date :: [Sondage] -> [(UTCTime, [(Candidat, Double)])]
sondage2date xs = concat ( map (dureeSondage) xs )
    where
        dureeSondage (Sondage _ begin end _ _ _ _ is) = case end of
                    Nothing   -> zip (timesBetween begin begin D) (repeat is)
                    Just end' -> zip (timesBetween begin end'  D) (repeat is)

toMap :: forall k a. Ord k => [(k, [a])] -> M.Map k [a]
toMap = Prelude.foldr (\s -> M.insertWith (++) (fst s) (snd s)) M.empty 

toMap' :: forall k a. Ord k => [(k, a)] -> M.Map k [a]
toMap' = Prelude.foldr (\s -> M.insertWith (++) (fst s) ([snd s])) M.empty 

mapSondages :: [Sondage] -> M.Map UTCTime (M.Map Candidat Double)
mapSondages sondage = M.map (M.map mean) $ M.map toMap' $ toMap (sondage2date sondage)
    where
        mean :: [Double] -> Double
        mean xs = (sum xs) / (fromIntegral $ length xs)

predict :: (Double -> Double) -> Int -> [(UTCTime, Double)]
predict f days = predict' sondages f days

predict' :: [Sondage] -> (Double -> Double) -> Int -> [(UTCTime, Double)]
predict' sondes f days = zip dates $ Prelude.map (\x -> f $ a' + b' * x) [1.. (fromIntegral n)]
    where
        sondes' = M.toAscList (mapSondages sondes)
        firstDate = fst $ head sondes'
        n = length sondes' + days
        dates = timesAfter n D firstDate
        (a', b') = linearRegression (U.fromList a) (U.fromList b)
        (a, b) = unzip $ zip ([1..] :: [Double]) $  Prelude.map snd $ concat $ Prelude.map M.toList $ Prelude.map snd sondes'
