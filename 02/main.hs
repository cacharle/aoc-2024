import           Data.List   (sort)
import           Text.Printf (printf)

{-# LANGUAGE TypeApplications #-}

main :: IO ()
main = do
    s <- map words . lines <$> readFile "input"
    let records = (map . map) (read @Int) s
        result1 = length $ filter isValidRecord records
        result2 = length $ filter isValidRecordTolerate records
    printf "Part 1: %d\n" result1
    printf "Part 2: %d\n" result2


isValidRecordTolerate :: [Int] -> Bool
isValidRecordTolerate r = inner 1 r
    where inner i r
           | i > length r = False
           | isValidRecord (take (i-1) r ++ drop i r) = True
           | otherwise = inner (i+1) r

isValidRecord :: [Int] -> Bool
isValidRecord r =
    (sort r == r || (reverse . sort) r == r)
    && (all (\d -> abs d `elem` [1..3]) $ zipWith (-) r (tail r))
