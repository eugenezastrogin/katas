-- https://www.codewars.com/kata/rail-fence-cipher-encoding-and-decoding/train/haskell
-- Create two functions to encode and then decode a string using
-- the Rail Fence Cipher. This cipher is used to encode a string by placing
-- each character successively in a diagonal along a set of "rails".

module RailFenceCipher.Kata (encode, decode) where

import           Data.List                      ( sortBy )
import           Data.Function                  ( on )

main :: IO ()
main = do
  print $ "Hello, World!" `encode` 3 `decode` 3

decode :: [a] -> Int -> [a]
decode w n = reverse $ snd $ selector w n

encode :: [a] -> Int -> [a]
encode w n = map snd $ sortBy (compare `on` fst) $ zip (wheel n) w

selector :: [a] -> Int -> ([[a]], [a])
selector w n =
  foldl
      (\(src, dst) x ->
        (
          let rest = tail (src !! x)
          in replaceNth x rest src,
          head (src !! x) : dst
        )
      )
      (splitByRail (charsByRail w n) w, mempty)
    $ take (length w) $ wheel n

splitByRail :: [Int] -> [a] -> [[a]]
splitByRail []       _ = []
splitByRail (x : xs) w = take x w : (splitByRail xs $ drop x w)

charsByRail :: [a] -> Int -> [Int]
charsByRail w n = map charsInRail [0 .. n - 1]
  where charsInRail x = length $ filter (== x) $ take (length w) $ wheel n

wheel :: Int -> [Int]
wheel n = cycle $ [0 .. n - 1] ++ [n - 2, n - 3 .. 1]

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x : xs) | n == 0    = newVal : xs
                             | otherwise = x : replaceNth (n - 1) newVal xs
