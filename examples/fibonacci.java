// This file is part of J--

// J-- is free software: you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option) any later
// version.

// J-- is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along with
// J--. If not, see <http://www.gnu.org/licenses/>.

// Copyright 2022 Luca Padovani

// recursive version, exponential time
int recursive_fibo(int n) {
  return n <= 1 ? n : recursive_fibo(n - 1) + recursive_fibo(n - 2);
}

// iterative version, linear time
int iterative_fibo(int n) {
  int a = 0, b = 1;
  while (n > 0) {
    int c = a + b;
    a = b;
    b = c;
    n--;
  }
  return a;
}

// identity matrix 2x2
int[][] identity() {
  return new int[][]
    { { 1, 0 }
    , { 0, 1 } };
}

// matrix multiplication a*b
int[][] mul(int[][] a, int[][] b) {
  return new int[][]
    { { a[0][0] * b[0][0] + a[0][1] * b[1][0], a[0][0] * b[0][1] + a[0][1] * b[1][1] }
    , { a[1][0] * b[0][0] + a[1][1] * b[1][0], a[1][0] * b[0][1] + a[1][1] * b[1][1] } };
}

// matrix exponentiation a^n
int[][] pow(int[][] a, int n) {
  if (n == 0) return identity();
  else {
    int[][] b = pow(a, n / 2);
    return n % 2 == 0 ? mul(b, b) : mul(a, mul(b, b));
  }
}

// efficient version, logarithmic
int efficient_fibo(int n) {
  int[][] a = new int[][]
    { { 1, 1 }
    , { 1, 0 } };
  int[][] b = pow(a, n);
  return b[0][1];
}

double round(double n) {
  return (int) (n * 1000.0d) / 1000.0d;
}

void test(int n) {
  double start = milliseconds();
  for (int i = 0; i < n; i++)
    println("Fibonacci(" + i + ") = " + recursive_fibo(i) + ", " + iterative_fibo(i) + ", " + efficient_fibo(i));
  double stop = milliseconds();
  println("Elapsed time = " + round(stop - start) + "ms");
}

test(40);
