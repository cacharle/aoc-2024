import Control.Arrow (first, second)
import Data.Char  (digitToInt, intToDigit)
import Data.List  (elemIndices, findIndex, findIndices)
import Data.Maybe (fromJust, fromMaybe)

-- 00...111...2...333.44.5555.6666.777.888899
-- 0099811188827773336446555566..............
--
--
-- 00...111...2...333.44.5555.6666.777.888899
-- 0099.111...2...333.44.5555.6666.777.8888..
-- 0099.1117772...333.44.5555.6666.....8888..
-- 0099.111777244.333....5555.6666.....8888..
-- 00992111777.44.333....5555.6666.....8888..


p :: [Int] -> String
p xs = [if x == -1 then '.' else intToDigit x | x <- xs]

main :: IO ()
main = do
    -- content <- readFile "input"
    content <- readFile "example"
    let disk = map digitToInt content
        decompressed = concat $ zipWith
            (\count i -> replicate count (if odd i then -1 else i `div` 2))
            disk [0..]
        emptySpaceCount = length $ elemIndices (-1) decompressed
        shifted = take (length decompressed - emptySpaceCount)
                  $ fixDisk decompressed (reverse decompressed)
        checksum = sum . zipWith (*) [0 ..]
    -- print $ p shifted
    print . checksum $ shifted
    let two = fst $ fixDisk2 decompressed (reverse decompressed)
    putStrLn . ("original: " <>) . p $ decompressed
    putStrLn . ("result:   " <>) . p $ two
    putStrLn . ("expected: " <>) $ "00992111777.44.333....5555.6666.....8888.."
    print . checksum $ two

fixDisk :: [Int] -> [Int] -> [Int]
fixDisk decompressed []        = decompressed
fixDisk decompressed (-1:rest) = fixDisk decompressed rest
fixDisk (-1:ds) (current:rest) = fixDisk (current:ds) rest
fixDisk (d:ds) xs              = d : fixDisk ds xs

-- TODO: make seconds argument groups with associated indices
-- and make sure the group index isn't before it's initial position
fixDisk2 :: [Int] -> [Int] -> ([Int], [Int])
fixDisk2 decompressed (-1:rest) = fixDisk2 decompressed rest
fixDisk2 decompressed@(-1:_) foo
    | nextF == foo = result
    | otherwise = fixDisk2 nextD nextF
 where
    result@(nextD, nextF) = fillOneFreeSpan decompressed foo
    fillOneFreeSpan [] foo = ([], foo)
    fillOneFreeSpan d [] = (d, [])
    fillOneFreeSpan decompressed@(-1 : _) foo@(current : _)
        | emptyCount >= nonEmptyCount =
              ( replicate nonEmptyCount current
                ++ replicate (emptyCount - nonEmptyCount) (-1)
                ++ ds'
              , foo'
              )
        | otherwise = second (fileBlocks ++) $ fillOneFreeSpan decompressed foo'
     where
        (emptyCount, ds') = first length $ span (== -1) decompressed
        (fileBlocks, foo') = span (== current) foo
        nonEmptyCount = length fileBlocks
fixDisk2 (d:ds) foo = first (d:) $ fixDisk2 ds foo















-- fixDisk decompressed (current:rest) =
--         fixDisk (replaceFirstEmpty current decompressed) rest
--     where
--         replaceFirstEmpty x xs =
--             let i = fromMaybe (length xs) $ findIndex (==(-1)) xs
--             in (take i xs) ++ [x] ++ (drop (i+1) xs)
