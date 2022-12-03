module Day03 where
import           Data.Char                      ( ord )
import           Data.List                      ( foldr
                                                , group
                                                , intersect
                                                , sort
                                                )

pair :: [Char] -> (String, String)
pair x = splitAt (div (length x) 2) x

unique :: Ord a => [a] -> [a]
unique lis = map head . group $ sort lis

uniquePair :: Ord a => ([a], [a]) -> ([a], [a])
uniquePair p = (a, b)  where
    a = unique $ fst p
    b = unique $ snd p

sharedItem :: Eq a => ([a], [a]) -> a
sharedItem (a, b) = case overlap of
    [item] -> item
    _      -> error "malformed"
    where overlap = a `intersect` b

priority :: Char -> Int
priority char | 'a' <= char && char <= 'z' = ord char - 96
              | 'A' <= char && char <= 'Z' = ord char - 38
              | otherwise                  = error "malformed"

trios :: [String] -> [[String]]
trios (a : b : c : d) = [a, b, c] : trios d
trios []              = []
trios _               = error "malformed"

intersection :: [String] -> Char
intersection lis = case overlap of
    [c] -> c
    _   -> error "malformed"
    where overlap = foldr intersect (head lis) lis


main :: IO ()
main = do
    content <- readFile "data/day03.txt"
    -- content <- readFile "data/day03test.txt"
    let p1 = sum . map (priority . sharedItem . uniquePair . pair) $ lines
            content
        p2 = sum . map (priority . intersection . map unique) . trios $ lines
            content
    print p1
    print p2
