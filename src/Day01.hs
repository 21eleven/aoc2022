module Day01 where
import           Data.List                      ( groupBy
                                                , reverse
                                                , sort
                                                )

isSplit :: String -> Bool
isSplit = (== "")

group :: [String] -> [[String]] -> [[String]]
group input acc = case input of
    [] -> acc
    ("" : rest) -> group rest acc
    input -> group (dropWhile (/= "") input) $ takeWhile (/= "") input : acc


main :: IO ()
main = do
    -- content <- readFile "data/day01test.txt"
    content <- readFile "data/day01.txt"
    let grouped = group (lines content) []
    let elves   = map (sum . map (read :: String -> Int)) grouped
    print $ foldl max 0 elves
    print $ sum . take 3 . reverse $ sort elves

