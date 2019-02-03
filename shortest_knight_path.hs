-- https://www.codewars.com/kata/shortest-knight-path/train/haskell
-- Given two different positions on a chess board, find the least number of
-- moves it would take a knight to get from one to the other.
-- The positions will be passed as two arguments in algebraic notation.
-- For example, knight("a3", "b5") should return 1.
-- The knight is not allowed to move off the board. The board is 8x8.

module ShortestKnightPath.Kata (knight) where

import qualified Data.Set as Set

knight :: String -> String -> Int
knight start finish = length $ takeWhile finishNotReached moves
  where finishNotReached = \(x, _) -> not $ elem finish x
        moves = iterate validMapper ([start], Set.empty)

onBoard :: String -> Bool
onBoard (x:y:_) = elem x ['a'..'h'] && elem y ['1'..'8']

validMoves :: String -> [String]
validMoves (x:y:_) = filter onBoard [
  [(pred . pred) x, succ y],
  [(pred . pred) x, pred y],
  [pred x, (succ . succ) y],
  [pred x, (pred . pred) y],
  [succ x, (succ . succ) y],
  [succ x, (pred . pred) y],
  [(succ . succ) x, succ y],
  [(succ . succ) x, pred y]
 ]

validMapper :: ([String], Set.Set String) -> ([String], Set.Set String)
validMapper (current, visited) = (reachable, visitedUpdated)
  where reachable = filter notYetVisited $ foldr reducer [] current
        notYetVisited = \x -> Set.notMember x visited
        reducer = \cur acc -> validMoves cur ++ acc
        visitedUpdated = Set.union visited $ Set.fromList current
