module Day06 where
import           Data.List                      ( group
                                                , sort
                                                )

findStart :: Int -> Int -> [Char] -> String -> Int
findStart _ _ _ [] = error "malformed"
findStart len idx window input
    | idx < len = findStart len (idx + 1) (head input : window) (tail input)
    | (length . group $ sort window) == len = idx
    | otherwise = findStart len
                            (idx + 1)
                            (head input : take (len - 1) window)
                            (tail input)

main :: IO ()
main = do
    -- content <- readFile "data/day06test.txt"
    content <- readFile "data/day06.txt"
    print $ map (findStart 4 0 []) $ lines content
    print $ map (findStart 14 0 []) $ lines content

