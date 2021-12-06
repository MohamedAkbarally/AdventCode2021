import System.IO
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

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
        u = [0..8]

daysPassed :: Int -> Map Int Int -> Map Int Int
daysPassed x m = n
    where
        n = update 8 10 m

main :: IO()
main = do
    ls <- fmap Text.lines (Text.readFile "./data/day_6/input.txt")
    print $ daysPassed 1 $ parseInput $ head ls





