// https://www.codewars.com/kata/shortest-knight-path/train/javascript
// Given two different positions on a chess board, find the least number of
// moves it would take a knight to get from one to the other.
// The positions will be passed as two arguments in algebraic notation.
// For example, knight("a3", "b5") should return 1.
// The knight is not allowed to move off the board. The board is 8x8.

'use strict';

// Helper functions

// equals :: (a, b) -> Boolean
function equals(arg1, arg2) {
  return arg1 === arg2;
}

// identity :: (a) -> a
const identity = x => x;

// isNull :: (a) -> Boolean
function isNull(val) {
  return val === null;
}

// isNotNull :: (a) -> Boolean
const isNotNull = val => !isNull(val);

// Maps algebraic notation to matrix indices
// algToMatrix :: String -> [Number, Number] | null
function algToMatrix(cell) {

  // mapX :: String -> Number | null
  function mapX(x) {
    const isCapital = (c) => (72 >= c && c >= 65);
    const isLower = (c) => (104 >= c && c >= 97);

    if (!isCapital(x) && !isLower(x)) {
      return null;
    }
    return isCapital(x) ? (x - 65) : (x - 97);
  }

  // mapY :: Number -> Number | null
  function mapY(y) {
    const isValid = (c) => (9 >= c && c >= 0);

    return isValid(y) ? y - 1 : null;
  }

  const [x, y] = cell.split('');

  const map = [
    mapX(x.charCodeAt(0)),
    mapY(parseInt(y)),
  ];

  return map.every(isNotNull) ? map : null;
}

// Maps matrix indices to algebraic notation
// matrixToAlg :: [Number, Number] -> String
function matrixToAlg(cell) {
  const [x, y] = cell;

  return [
    String.fromCharCode(x + 97),
    y + 1,
  ].join('');

}

// Returns whether a provided position fits into chess matrix
// onBoard :: [Number, Number] -> Boolean
function onBoard(position) {
  const inGrid = x => x >= 0 && x < 8
  return position.every(inGrid);
}

// Returns a list of legit knight moves from current position
// validMoves :: [Number, Number] -> [[Number, Number],..]
function validMoves(position) {
  const cellShift = [
    [-2,  1],
    [-2, -1],
    [-1,  2],
    [-1, -2],
    [1,  2],
    [1, -2],
    [2,  1],
    [2, -1],
  ];

  return cellShift
    .map(x => [position[0] + x[0], position[1] + x[1]])
    .filter(onBoard)
}

// Main function
// knight :: (String, String) -> Number | null
function knight(start, finish) {
  console.log(validMoves(algToMatrix(finish)).map(matrixToAlg), finish);
  return null;
}

const arr = [
  ['a1', 'c1', 2],
  ['a1', 'f1', 3],
  ['a1', 'f3', 3],
  ['a1', 'f4', 4],
  ['a1', 'f7', 5]
];

for (let i of arr) {
  equals(knight(i[0], i[1]), i[2])
};
