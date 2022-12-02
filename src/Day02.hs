module Day02 where

data Move = Rock | Paper | Scissors deriving Show

move :: Char -> Move
move 'A' = Rock
move 'X' = Rock
move 'B' = Paper
move 'Y' = Paper
move 'C' = Scissors
move 'Z' = Scissors
move _   = error "malformed"

val :: Move -> Int
val x = case x of
    Rock     -> 1
    Paper    -> 2
    Scissors -> 3

game :: (Move, Move) -> Int
game (elf, self) = case (elf, self) of
    (Rock    , Paper   ) -> 6
    (Rock    , Rock    ) -> 3
    (Rock    , Scissors) -> 0
    (Paper   , Paper   ) -> 3
    (Paper   , Rock    ) -> 0
    (Paper   , Scissors) -> 6
    (Scissors, Paper   ) -> 0
    (Scissors, Rock    ) -> 6
    (Scissors, Scissors) -> 3

parse :: String -> (Move, Move)
parse (l : _ : r : _) = (move l, move r)
parse _               = error "malformed"

data Outcome = Win | Lose | Draw
parse' :: String -> (Move, Outcome)
parse' (l : _ : r : _) = (move l, outcome r)
parse' _               = error "malformed"

outcome :: Char -> Outcome
outcome x = case x of
    'X' -> Lose
    'Y' -> Draw
    'Z' -> Win
    _   -> error "malformed"

game' :: (Move, Outcome) -> (Move, Move)
game' (elf, out) = case (elf, out) of
    (Rock    , Win ) -> (Rock, Paper)
    (Rock    , Draw) -> (Rock, Rock)
    (Rock    , Lose) -> (Rock, Scissors)
    (Paper   , Draw) -> (Paper, Paper)
    (Paper   , Lose) -> (Paper, Rock)
    (Paper   , Win ) -> (Paper, Scissors)
    (Scissors, Lose) -> (Scissors, Paper)
    (Scissors, Win ) -> (Scissors, Rock)
    (Scissors, Draw) -> (Scissors, Scissors)

score :: (Move, Move) -> Int
score (elf, self) = game (elf, self) + val self

main :: IO ()
main = do
    -- content <- readFile "data/day02test.txt"
    content <- readFile "data/day02.txt"
    print $ sum . map (score . parse) $ lines content
    print $ sum . map (score . game' . parse') $ lines content
