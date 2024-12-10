{-# LANGUAGE TypeApplications #-}

import Data.Char  (digitToInt, intToDigit)
import Data.Maybe (fromJust, fromMaybe)
import Data.List  (findIndex, findIndices)

-- 00...111...2...333.44.5555.6666.777.888899
-- 0099811188827773336446555566..............
--
--
-- 00...111...2...333.44.5555.6666.777.888899
-- 0099.111...2...333.44.5555.6666.777.8888..
-- 0099.1117772...333.44.5555.6666.....8888..
-- 0099.111777244.333....5555.6666.....8888..
-- 00992111777.44.333....5555.6666.....8888..

import Control.Arrow (first)

p :: [Int] -> String
p xs = [if x == -1 then '.' else intToDigit x | x <- xs]

main :: IO ()
main = do
    content <- readFile "example"
    let disk = map digitToInt content
        decompressed = concat $ zipWith
            (\count i -> replicate count (if odd i then  -1 else i `div` 2))
            disk [0..]
        emptySpaceCount = length $ findIndices (== -1) decompressed
        shifted = take (length decompressed - emptySpaceCount)
                  $ fixDisk decompressed (reverse decompressed)
    -- print $ p decompressed
    -- print $ p shifted
    let checksum = sum $ zipWith (*) shifted [0..]
    print checksum

fixDisk :: [Int] -> [Int] -> [Int]
fixDisk decompressed []        = decompressed
fixDisk decompressed (-1:rest) = fixDisk decompressed rest
fixDisk (-1:ds) (current:rest) = fixDisk (current:ds) rest
fixDisk (d:ds)  xs             = d:(fixDisk ds xs)


-- 8:05 PM hysm's vault - but because you're dropping everything that doesn't match we delete everything bigger than two
-- 8:09 PM hysm's vault - you do `drop nonEmptyCount` in fixDisk2's return, whether the file was able to fill the spot or not
-- 8:11 PM hysm's vault - the drop there means that when we're trying to fill the 1 block gap we'll throw away every file until we reach 2
-- 8:12 PM hysm's vault - and then we'll fit the next free block with 1s
-- 8:15 PM hysm's vault - fixDisk2 should fill one empty spot and return the remaining filler chars so when concat with them the files we couldn't use
-- 8:15 PM hysm's vault - we* concat
-- 8:18 PM hysm's vault - if you make it return decompressed as well as foo on every invocation you'll be able to skip the files that are too big in an inner iteration
-- 8:18 PM hysm's vault - then take the new `foo` value and concat it with the files you skipped

-- TODO: make seconds argument groups with associated indices
-- and make sure the group index isn't before it's initial position
fixDisk2 :: [Int] -> [Int] -> [Int]
fixDisk2 decompressed []        = decompressed
fixDisk2 decompressed (-1:rest) = fixDisk2 decompressed rest
fixDisk2 decompressed@(-1:ds) foo@(current:rest) =
    let (emptyCount, ds') = first length $ span (== -1) decompressed
        nonEmptyCount = 1 + (length $ takeWhile (== current) rest)
        (nextDecompressed, nextFoo) =
            if emptyCount >= nonEmptyCount
            then
                 (replicate nonEmptyCount current
                 ++ replicate (emptyCount - nonEmptyCount) (-1)
                 ++ ds'
                 , drop nonEmptyCount foo
                 )
            else (decompressed, foo)
    in fixDisk2 nextDecompressed nextFoo
fixDisk2 (d:ds)  xs             = d:(fixDisk2 ds xs)





















-- fixDisk decompressed (current:rest) =
--         fixDisk (replaceFirstEmpty current decompressed) rest
--     where
--         replaceFirstEmpty x xs =
--             let i = fromMaybe (length xs) $ findIndex (==(-1)) xs
--             in (take i xs) ++ [x] ++ (drop (i+1) xs)




