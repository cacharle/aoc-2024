module Main where

import Data.List (nub, tails, inits, (\\), delete)
import Debug.Trace (trace)
import qualified Data.Set as Set
import Text.Printf (printf)

type Position = (Int, Int)

data Direction = DUp | DDown | DLeft | DRight
    deriving Show


-- To get a iteration count: 2>| awk '{print NR}'
--
-- Part 2 took 32min to solve, Ideas for improvement
-- - [x] Only try visited cell instead of all around visited
--      ==> 19min with this + inits instead of tails.reverse
-- - [x] use Data.Set for obstacles to be able to check `elem` faster
--      ==> slightly faster (maybe HashSet would be better)
main :: IO ()
main = do
    content <- readFile "./input"
    let positions = concatMap
                    (\(l, i) -> zipWith (\c j -> (c, (i, j))) l [0..])
                    $ zip (lines content) [0..]
        maxRow    = maximum $ map (fst . snd) positions
        maxCol    = maximum $ map (snd . snd) positions
        obstacles = Set.fromList $ map snd $ filter ((=='#') . fst) positions
        guard     = head $ map snd $ filter ((=='^') . fst) positions
        (visited', _, _) = guardRun maxRow maxCol obstacles guard
        visited = nub visited'
        result1 = length visited
        emptyPositions = map snd positions \\ (guard:(Set.toList obstacles))
        candidateObstacles = delete guard visited
        result2 = length $ filter (\(ps,_,_) -> isInLoop ps) $ map
            (\o -> trace "#" $ guardRun maxRow maxCol (Set.insert o obstacles) guard)
            candidateObstacles
    -- print $ length emptyPositions
    -- print $ length candidateObstacles
    -- print $ candidateObstacles
    printf "Part 1: %d\n" result1
    printf "Part 2: %d\n" result2

guardRun :: Int
         -> Int
         -> Set.Set Position
         -> Position
         -> ([Position], Direction, Position)
guardRun maxRow maxCol obstacles guard = until
        (\(ps, _, (gi,gj)) ->
            gi < 0 || gj < 0 || gi > maxRow || gj > maxCol
            || (length ps > 10000 && length ps `mod` 1000 == 0) && isInLoop ps
            )
        (guardNext obstacles)
        ([], DUp, guard)


isInLoop :: [Position] -> Bool
-- reverse first since we add the visisted positions at the *start* of the list
isInLoop = any isInLoop' . inits
    where isInLoop' ps = (length ps >= 2)
                         && (even (length ps))
                         && (uncurry (==) $ splitAt (length ps `div` 2) ps)

           -- otherwise = let mid = length positions `div` 2
           --               in take mid positions == drop mid positions


guardNext :: Set.Set Position
          -> ([Position], Direction, Position)
          -> ([Position], Direction, Position)
guardNext obstacles (ps, d, guard@(gi, gj)) =
    case d of
      DUp    -> guardNext' (-1,  0)
      DDown  -> guardNext' ( 1,  0)
      DLeft  -> guardNext' ( 0, -1)
      DRight -> guardNext' ( 0,  1)
    where guardNext' (mi, mj) =
            let candidate = (gi + mi, gj + mj)
            in if candidate `Set.member` obstacles
               then (ps, turnRight d, guard)
               else (guard:ps, d, candidate)
          turnRight DUp    = DRight
          turnRight DRight = DDown
          turnRight DDown  = DLeft
          turnRight DLeft  = DUp


