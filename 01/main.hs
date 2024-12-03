import Data.List
import Text.Printf

{-# LANGUAGE TypeApplications #-}

main :: IO ()
main = do
    s <- map words . lines <$> readFile "input"
    let location_ids = map (\[l1, l2] -> (read @Int l1, read @Int l2)) s
        (lst1, lst2) = unzip location_ids
        lst1Sorted = sort lst1
        lst2Sorted = sort lst2
        result1 = sum $ zipWith (\a b -> abs $ a - b) lst1Sorted lst2Sorted
        result2 = sum $ map (\id -> id * (length $ filter (== id) lst2)) lst1
    printf "Part 1: %d\n" result1
    printf "Part 2: %d\n" result2
