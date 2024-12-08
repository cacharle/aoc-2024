import           Data.Function (on)
import           Data.List     (groupBy, nub, sortOn)
import           Text.Printf   (printf)

main :: IO ()
main = do
    content <- readFile "input"
    let positions = concatMap
                    (\(l, i) -> zipWith (\c j -> (c, (i, j))) l [0..])
                    $ zip (lines content) [0..]
        maxRow = maximum $ map (fst . snd) positions
        maxCol = maximum $ map (snd . snd) positions
        antennas = filter ((/='.') . fst) positions
        antennasGrouped = (map . map) snd $ groupBy ((==) `on` fst) (sortOn fst antennas)
        result1 = length $ allAntiNodes False maxRow maxCol antennasGrouped
        result2 = length $ allAntiNodes True  maxRow maxCol antennasGrouped
    printf "Part 1: %d\n" result1
    printf "Part 2: %d\n" result2


allAntiNodes :: Bool -> Int -> Int -> [[(Int, Int)]] -> [(Int, Int)]
allAntiNodes harmonic maxRow maxCol antennasGrouped =
    nub $ concatMap (antiNodes harmonic maxRow maxCol) antennasGrouped


antiNodes :: Bool -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
antiNodes harmonic maxRow maxCol = filter (inBound maxRow maxCol) . antiNodes'
    where
        antiNodes' :: [(Int, Int)] -> [(Int, Int)]
        antiNodes' [] = []
        antiNodes' [a] = []
        antiNodes' (a1:a2:as) =
            antiNode harmonic maxRow maxCol a1 a2
            ++ antiNodes' (a1:as) ++ antiNodes' (a2:as)


antiNode :: Bool
         -> Int
         -> Int
         -> (Int, Int)
         -> (Int, Int)
         -> [(Int, Int)]
antiNode harmonic maxRow maxCol a1@(a1i, a1j) a2@(a2i, a2j) =
    let (di, dj) = (a1i - a2i, a1j - a2j) in inner harmonic di dj
    where
        inner False di dj = [(a1i + di, a1j + dj), (a2i - di, a2j - dj)]
        inner True  di dj = antiNodesHarmonic di dj a1 ++ antiNodesHarmonic (-di) (-dj) a2
        antiNodesHarmonic di dj (ai, aj) =
            takeWhile (inBound maxRow maxCol) [(ai + n * di, aj + n * dj) | n <- [0..]]


inBound :: Int -> Int -> (Int, Int) -> Bool
inBound maxRow maxCol (i,j) = i >= 0 && j >= 0 && i <= maxRow && j <= maxCol
