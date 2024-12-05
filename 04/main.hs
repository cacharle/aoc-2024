import           Data.List   (isPrefixOf, reverse, transpose)
import           Data.Maybe  (fromMaybe)
import           Text.Printf (printf)

main :: IO ()
main = do
    content <- readFile "input"
    let grid = lines content
        result1 = countWordGrid grid
        result2 = countCrosses grid
    printf "Part 1: %d\n" result1
    printf "Part 2: %d\n" result2


countCrosses :: [String] -> Int
countCrosses g = length [()
                        | offR <- [0..(length g)]
                        , offC <- [0..(length (head g))]
                        , inner offR offC
                        ]
    where inner offR offC =
            isCorrectCross $ map ((take 3) . (drop offC)) $ take 3 (drop offR g)
          isCorrectCross cross =
            let mainDiagonal = diagonal 0 cross
                mainAntiDiagonal = diagonal 0 (map reverse cross)
            in (mainDiagonal == "MAS" || mainDiagonal == "SAM")
               && (mainAntiDiagonal == "MAS" || mainAntiDiagonal == "SAM")


countWordGrid :: [String] -> Int
countWordGrid g = let gridT  = transpose g
                      gridD  = diagonals g
                      gridAD = diagonals $ map reverse g
                  in count g + count gridT + count gridD + count gridAD
    where count g = sum $ map countWord g


diagonals :: [String] -> [String]
diagonals g = [diagonal d g | d <- [-(length g)..length g]]


diagonal :: Int -> [String] -> String
-- TODO: find a fancy way of removing the lambda
diagonal d g = zipWith (\r i -> fromMaybe '.' (r !? i)) g [d..]

-- Can't use the !? operator from Data.List because my base version is too low
(!?) :: [a] -> Int -> Maybe a
{-# INLINABLE (!?) #-}
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n


countWord :: String -> Int
countWord "" = 0
countWord s@(c:rest)
    | "XMAS" `isPrefixOf` s           = 1 + countWord rest
    | (reverse "XMAS") `isPrefixOf` s = 1 + countWord rest
    | otherwise                       = countWord rest
