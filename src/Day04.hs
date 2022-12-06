module Day04 where
import           Data.Char                      ( isDigit )
import           Data.List                      ( groupBy )

type PairPair = ((Int, Int), (Int, Int))

parse :: String -> PairPair
parse input = ((n a, n b), (n x, n y))
  where
    split                       = groupBy (\i j -> isDigit i == isDigit j) input
    n                           = read :: String -> Int
    [a, "-", b, ",", x, "-", y] = split

isWrap :: PairPair -> Bool
isWrap ((a, b), (x, y)) = a <= x && x <= b && a <= y && y <= b

p1Check :: PairPair -> Bool
p1Check (a, b) = isWrap (a, b) || isWrap (b, a)

p2Check :: PairPair -> Bool
p2Check (a, b) = isOverlap (a, b) || isOverlap (b, a)

isOverlap :: PairPair -> Bool
isOverlap ((a, b), (x, y)) = (a <= x && x <= b) || (a <= y && y <= b)


main :: IO ()
main = do
    -- content <- readFile "data/day04test.txt"
    content <- readFile "data/day04.txt"
    let parsed = map parse $ lines content
    let p1     = length $ filter p1Check parsed
    let p2     = length $ filter p2Check parsed
    print p1
    print p2

