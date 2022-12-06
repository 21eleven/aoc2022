import           Data.Maybe                     ( fromJust )
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import           System.Environment             ( getArgs )

dispatch =
    [ ("Day01", Day01.main)
    , ("Day02", Day02.main)
    , ("Day03", Day03.main)
    , ("Day04", Day04.main)
    , ("Day05", Day05.main)
    ]

main :: IO ()
main = do
    args <- getArgs
    let day    = head args
        mainFn = fromJust $ lookup day dispatch
    mainFn
