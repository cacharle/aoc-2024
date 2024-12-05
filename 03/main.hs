import           Control.Monad (guard)
import           Data.Char     (isDigit)
import           Data.List     (isPrefixOf)
import           Data.Maybe    (fromMaybe)
import           Text.Printf   (printf)

main :: IO ()
main = do
    content <- readFile "input"
    let result1 = parseCorruptedMemory True True content
        result2 = parseCorruptedMemory False True content
    printf "Part 1: %d\n" result1
    printf "Part 2: %d\n" result2


parseCorruptedMemory :: Bool -> Bool -> String -> Int
parseCorruptedMemory part1 toggle s = inner toggle s
    where inner toggle s@(_:rest)
            | "do()"      `isPrefixOf` s  = inner True rest
            | "don't()"   `isPrefixOf` s  = inner False rest
            | not ("mul(" `isPrefixOf` s) = inner toggle rest
            | otherwise = fromMaybe (inner toggle rest) $ do
                guard (part1 || toggle)
                (n1, c1:rest1) <- tryParseInt (drop 4 s)
                guard (c1 == ',')
                (n2, c2:rest2) <- tryParseInt rest1
                guard (c2 == ')')
                return $ (n1 * n2) + inner toggle rest2
          inner _ "" = 0


tryParseInt :: String -> Maybe (Int, String)
tryParseInt s = case span isDigit s of
                    ("", rest) -> Nothing
                    (n,  rest) -> Just ((read n), rest)


