{-# LANGUAGE TypeApplications #-}

main :: IO ()
main = do
    content <- readFile "input"
    let equations = map
            (\(r, os) -> (read @Int r, map (read @Int) $ words (tail os)))
            $ map (break (==':')) $ lines content
        result1 = sum $ map fst $ filter isValidEquation equations
    print result1


isValidEquation :: (Int, [Int]) -> Bool
isValidEquation (result, operands) =
        -- Reverse since the computations are left to right only
        any (==result) (possibleComputations $ reverse operands)
    where
        possibleComputations :: [Int] -> [Int]
        possibleComputations [] = []
        possibleComputations [o] = [o]
        possibleComputations (o:os) = let computed = possibleComputations os
                                      in map (o+) computed
                                         ++ map (o*) computed
                                         ++ map (o `concatDigits`) computed
        -- Arguments are flipped since we to left to right
        concatDigits y x = read (show x ++ show y)



