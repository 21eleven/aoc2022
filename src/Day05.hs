module Day05 where

parseRow :: String -> [Char]
parseRow ('['       : chr : ']' : row) = chr : parseRow row
parseRow (' ' : '[' : chr : ']' : row) = chr : parseRow row
parseRow (' ' : ' ' : ' ' : ' ' : row) = ' ' : parseRow row
parseRow (' '       : ' ' : ' ' : row) = ' ' : parseRow row
parseRow []                            = []
parseRow _                             = error "malformed"

main :: IO ()
main = do
    -- content <- readFile "data/day05test.txt"
    content <- readFile "data/day05.txt"
    let moveLines =
            reverse . takeWhile (\ln -> take 1 ln == "m") . reverse $ lines
                content
            -- reverse . takeWhile (\(m : _) -> m == 'm') . reverse $ lines content
    let
        stackLines =
            map parseRow
                . reverse
                . takeWhile (\ln -> take 2 ln /= " 1")
                $ lines content
    print moveLines
    print stackLines

