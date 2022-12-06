{-# LANGUAGE NamedFieldPuns #-}
module Day05 where
import           Data.Char                      ( isDigit )
import           Data.List                      ( groupBy )
import           Data.Map                       ( Map
                                                , adjust
                                                , elems
                                                , empty
                                                , fromList
                                                , insert
                                                , insertWith
                                                , lookup
                                                )
import           Prelude                 hiding ( lookup )

data Move = Move
    { amt  :: Int
    , from :: Int
    , to   :: Int
    }
    deriving Show

parseMove :: [String] -> Move
parseMove [_, amt, _, from, _, to] = Move (int amt) (int from) (int to)
    where int = read :: String -> Int
parseMove _ = error "malformed"

parseRow :: String -> [Char]
parseRow ('['       : chr : ']' : row) = chr : parseRow row
parseRow (' ' : '[' : chr : ']' : row) = chr : parseRow row
parseRow (' ' : ' ' : ' ' : ' ' : row) = ' ' : parseRow row
parseRow (' '       : ' ' : ' ' : row) = ' ' : parseRow row
parseRow []                            = []
parseRow _                             = error "malformed"


insertRow :: Map Int String -> [(Int, [Char])] -> Map Int String
insertRow = foldl (\ma (k, v) -> insertWith (++) k v ma)

craneLift :: Map Int String -> Move -> Map Int String
craneLift m Move { amt = 0 }       = m
craneLift m Move { amt, from, to } = craneLift updated $ Move (amt - 1) from to
  where
    updated                  = adjust tail from $ insertWith (++) to [crate] m
    Just (crate : fromStack) = lookup from m

craneLift9001 :: Map Int String -> Move -> Map Int String
craneLift9001 m Move { amt, from, to } = insert from fromStack
    $ insertWith (++) to crates m
    where Just (crates, fromStack) = splitAt amt <$> lookup from m

main :: IO ()
main = do
    -- content <- readFile "data/day05test.txt"
    content <- readFile "data/day05.txt"
    let moveLines =
            reverse . takeWhile (\ln -> take 1 ln == "m") . reverse $ lines
                content
    let moves =
            map (parseMove . groupBy (\x y -> isDigit x == isDigit y)) moveLines
    let stackLines =
            map
                    ( map (\(k, v) -> (k, [v]))
                    . filter (\(k, v) -> v /= ' ')
                    . zip [1 ..]
                    . parseRow
                    )
                . reverse
                . takeWhile (\ln -> take 2 ln /= " 1")
                $ lines content
    let stacks = foldl insertRow empty stackLines
    let p1     = map head . elems $ foldl craneLift stacks moves
    let p2     = map head . elems $ foldl craneLift9001 stacks moves
    print p1
    print p2


