-- https://www.codewars.com/kata/snail/train/haskell
-- Given an n x n array, return the array elements arranged
-- from outermost elements to the middle element, traveling clockwise.

module Snail where

main :: IO ()
main = do
  putStr "Expected: [], got: "
  print $ snail [[]]
  putStr "Expected: [1,2,3,6,9,8,7,4,5], got: "
  print $ snail [[1,2,3],[4,5,6],[7,8,9]]

snail :: [[Int]] -> [Int]
snail [[]] = []
snail m = map (\(i, j) -> m !! i !! j) $ moveMapper 'R' (length m) (0, 0) []

nextDir :: Char -> Char
nextDir dir
  | dir == 'R' = 'D'
  | dir == 'D' = 'L'
  | dir == 'L' = 'U'
  | dir == 'U' = 'R'

mv :: Char -> Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
mv dir size (i, j) visited
  | dir == 'R' = notVisited $ map ((,) i) [j..size - 1]
  | dir == 'D' = notVisited $ map (flip (,) j) [i..size - 1]
  | dir == 'L' = notVisited $ map ((,) i) [j, (j - 1)..0]
  | dir == 'U' = notVisited $ map (flip (,) j) [i, (i - 1)..0]
  where notVisited = filter (flip notElem visited)

moveMapper :: Char -> Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
moveMapper dir size start visited =
  if length trav > 0
  then trav ++ moveMapper (nextDir dir) size (last trav) (trav ++ visited)
  else trav
    where trav = mv dir size start visited
