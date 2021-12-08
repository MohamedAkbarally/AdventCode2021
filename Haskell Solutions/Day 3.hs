import System.IO
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List

parseDigits :: Text.Text -> [Int]
parseDigits ls = map (read . pure) (Text.unpack ls)


toDec :: [Int] -> Int
toDec [] = 0
toDec (x:xs) = x*2^(length xs) + toDec(xs) 

gammaRate :: [[Int]] -> Int
gammaRate x = toDec [fromEnum (i > length x `div` 2) | i <- s]
    where s = map (sum) (transpose x)

epsilonRate :: [[Int]] -> Int
epsilonRate x = toDec [fromEnum (i < length x `div` 2) | i <- s]
    where s = map (sum) (transpose x)


scubber :: (a -> a -> Bool) -> [[Int]] -> [Int]
scubber _ [x] = x
scubber f x = [s] ++ scubber f (map (tail) (filter (\n -> (head n) == s) x))
    where s = fromEnum $ sum (f (head (transpose x)) (length x `div` 2))


main :: IO()
main = do
    ls <- fmap Text.lines (Text.readFile "./data/day_3/input.txt")
    let xs = map parseDigits ls
    print ((gammaRate xs)*(epsilonRate xs))
    print $ toDec $ scubber (<) xs





