// https://www.codewars.com/kata/snail/train/javascript
// Given an n x n array, return the array elements arranged
// from outermost elements to the middle element, traveling clockwise.

function snail(matrix) {
  if (!matrix.length) return [];

  const[xs, ...xss] = matrix;

  return xs.concat(snail(transpose(xss).reverse()));
}

function transpose(a) {
  if (!a.length) return [];

  return Object.keys(a[0]).map(function(c) {
    return a.map(function(r) { return r[c]; });
  });
}
