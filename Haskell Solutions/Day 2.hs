import System.IO
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List

parseDirection :: Text.Text -> (String,Int)
parseDirection x = (init (init s), read (pure (last s)))
    where s = Text.unpack x

posHorizontal :: [(String, Int)] -> Int
posHorizontal [] = 0
posHorizontal (("forward", y):rest) = posHorizontal(rest) + y
posHorizontal ((_, y):rest) = posHorizontal(rest)

posVertical :: [(String, Int)] -> Int
posVertical [] = 0
posVertical (("down", y):rest) = posVertical(rest) + y
posVertical (("up", y):rest) = posVertical(rest) - y
posVertical ((_, y):rest) = posVertical(rest)

depth :: [(String, Int)] -> Int
depth [] = 0
depth (("forward", y):rest) = depth(rest) + (y*posVertical(rest))
depth ((_, y):rest) = depth(rest)


main :: IO()
main = do
    ls <- fmap Text.lines (Text.readFile "./data/day_2/input.txt")
    let h = posHorizontal (map parseDirection ls)
    let v = posVertical (map parseDirection ls)
    let d = depth (map parseDirection (reverse ls))
    putStr "Answer pt 1: "
    print $ h*v
    putStr "Answer pt 2: "
    print $ h*d



