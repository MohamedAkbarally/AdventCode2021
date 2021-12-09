import System.IO
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List

readInt :: Text.Text -> Int
readInt = read . Text.unpack

timesIncreasing :: [Int] -> Int
timesIncreasing [x] = 0
timesIncreasing (x:xx:xxs) =  fromEnum (x<xx) + timesIncreasing(xx:xxs)

windowed :: [Int] -> [Int]
windowed [x,xx] = []
windowed (x:xx:xxx:xxxs) = [x+xx+xxx] ++  windowed(xx:xxx:xxxs)


main :: IO()
main = do
    ls <- fmap Text.lines (Text.readFile "./data/day_1/input.txt")
    putStr "Answer pt 1: "
    print $ timesIncreasing $ map readInt ls
    putStr "Answer pt 2: "
    print $ timesIncreasing $ windowed $ map readInt ls


