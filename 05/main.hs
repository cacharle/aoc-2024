import           Data.List       (elemIndex, partition, permutations)
import           Data.List.Split (splitOn)
import           Text.Printf     (printf)

{-# LANGUAGE TypeApplications #-}

main :: IO ()
main = do
    content <- readFile "input"
    let ls = lines content
        (rules', _:updates') = span (elem '|') ls
        rules = map (\(x1:x2:_) -> (read @Int x1, read @Int x2)) $ map (splitOn "|") rules'
        updates = map (map (read @Int) <$> splitOn ",") updates'
        (validUpdates, invalidUpdates) = partition (validatesRules rules) updates
        -- Trying on all the permutations of the invalid updates didn't work due to time complexity being O(n!)
        fixedInvalid = map (fixUpdate rules) invalidUpdates
        result1 = sumOfMiddleElem validUpdates
        result2 = sumOfMiddleElem fixedInvalid
    printf "Part 1: %d\n" result1
    printf "Part 2: %d\n" result2
    where sumOfMiddleElem xs = sum $ map (\u -> u !! (length u `div` 2)) xs


validatesRules :: [(Int, Int)] -> [Int] -> Bool
validatesRules [] _ = True
validatesRules rules update = all validRule rules
    where validRule (lo,hi) =
            let i1 = elemIndex lo update
                i2 = elemIndex hi update
            in case (i1, i2) of
                (Nothing, _)       -> True
                (_, Nothing)       -> True
                (Just i1, Just i2) -> i1 < i2


fixUpdate :: [(Int, Int)] -> [Int] -> [Int]
fixUpdate rules = until (validatesRules rules)
                             (\u -> foldl (\fixed f -> f fixed) u (map fix rules))
    where fix (lo,hi) update =
            let i1 = elemIndex lo update
                i2 = elemIndex hi update
            in case (i1, i2) of
                (Nothing, _) -> update
                (_, Nothing) -> update
                (Just i1, Just i2) ->
                    if i1 < i2 then update else swapElementsAt i1 i2 update


-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

-- From: https://stackoverflow.com/questions/30551033/swap-two-elements-in-a-list-by-its-indices
-- Modified so that it supports the indices being out of order
swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt i j xs
    | i > j = swapElementsAt j i xs
    | otherwise = let elemI = xs !! i
                      elemJ = xs !! j
                      left = take i xs
                      middle = take (j - i - 1) (drop (i + 1) xs)
                      right = drop (j + 1) xs
                  in  left ++ [elemJ] ++ middle ++ [elemI] ++ right
