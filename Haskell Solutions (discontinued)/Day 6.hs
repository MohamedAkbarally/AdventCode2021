import System.IO
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

maybeToInt :: Maybe Int -> Int
maybeToInt (Just n) = n
maybeToInt Nothing = 0

-- func taken from https://stackoverflow.com/a/4981265
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parseInput :: Text.Text -> Map Int Int
parseInput ls = Map.fromList [ (i,length (filter (==i) s)) | i <- u]
    where
        s = map read (wordsWhen (==',') (Text.unpack ls)) :: [Int]
        u = [0..9]

daysPassed :: Int -> Map Int Int -> Int -> Int
daysPassed 0 m 0 = sum $ init $ Map.elems m
daysPassed x m 0 = daysPassed x (Map.insert 9 f m) 1
    where f = maybeToInt (Map.lookup 0 m)
daysPassed x m 7 = daysPassed x (Map.insert 6 (f+n) m) 8
    where 
        n = maybeToInt (Map.lookup 7 m)
        f = maybeToInt (Map.lookup 9 m)   
daysPassed x m 9 = daysPassed (x-1) (Map.insert 8 (maybeToInt (Map.lookup 9 m)) m) 0
daysPassed x m n = daysPassed x (Map.insert (n-1) (maybeToInt (Map.lookup n m)) m) (n+1) 

main :: IO()
main = do
    ls <- fmap Text.lines (Text.readFile "./data/day_6/input.txt")
    putStr "Answer pt 1: "
    print $ daysPassed 80 (parseInput (head ls)) 0
    putStr "Answer pt 2: "
    print $ daysPassed 256 (parseInput (head ls)) 0





