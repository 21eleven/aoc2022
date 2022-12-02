import           Data.Maybe                     ( fromJust )
import qualified Day01
import qualified Day02
import           System.Environment             ( getArgs )

dispatch = [("Day01", Day01.main), ("Day02", Day02.main)]

main :: IO ()
main = do
    args <- getArgs
    let day    = head args
        mainFn = fromJust $ lookup day dispatch
    mainFn
