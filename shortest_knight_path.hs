-- https://www.codewars.com/kata/shortest-knight-path/train/haskell
-- Given two different positions on a chess board, find the least number of
-- moves it would take a knight to get from one to the other.
-- The positions will be passed as two arguments in algebraic notation.
-- For example, knight("a3", "b5") should return 1.
-- The knight is not allowed to move off the board. The board is 8x8.

module ShortestKnightPath.Kata (knight) where

import qualified Data.Set as S

knight :: String -> String -> Int
knight start finish = length $ takeWhile finishNotReached moves
  where finishNotReached = \(x, _) -> notElem finish x
        moves = iterate validMapper (S.singleton start, S.empty)

onBoard :: String -> Bool
onBoard (x:y:_) = elem x ['a'..'h'] && elem y ['1'..'8']

validMoves :: String -> S.Set String
validMoves (x:y:_) = S.filter onBoard $ S.fromList
  [ [(pred . pred) x, succ y]
  , [(pred . pred) x, pred y]
  , [(succ . succ) x, succ y]
  , [(succ . succ) x, pred y]
  , [pred x, (succ . succ) y]
  , [pred x, (pred . pred) y]
  , [succ x, (succ . succ) y]
  , [succ x, (pred . pred) y]
  ]

validMapper :: (S.Set String, S.Set String) -> (S.Set String, S.Set String)
validMapper (current, visited) = (reachable, visitedUpdated)
  where reachable = S.filter notYetVisited $ foldr reducer S.empty current
        reducer = \cur acc -> S.union acc $ validMoves cur
        notYetVisited = \x -> S.notMember x visited
        visitedUpdated = S.union visited current
