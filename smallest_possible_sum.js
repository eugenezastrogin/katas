// https://www.codewars.com/kata/smallest-possible-sum/train/javascript
// Given an array X of positive integers, its elements are to be transformed
// by running the following operation on them as many times as required:
// if X[i] > X[j] then X[i] = X[i] - X[j]
// When no more transformations are possible, return its sum.

function solution(numbers) {
  return numbers.length * listGCD(numbers);
}

function listGCD(arr) {
  const n = arr.length;
  let x = Math.abs(arr[0]);

  for (let i = 1; i < n; i++) {
    y = Math.abs(arr[i]);

    while (x && y) {
      (x > y) ? x %= y : y %= x;
    }
    x += y;
  }
  return x;
}
