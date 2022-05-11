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

// crete identity matrix
int[][] identity(int n) {
  assert n > 0;
  int[][] m = new int[n][n];
  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      m[i][j] = i == j ? 1 : 0;
  return m;
}

// number of rows of matrix m
int rows(int[][] m) {
  return m.length;
}

// number of columns of matrix m
int columns(int[][] m) {
  return m[0].length;
}

// row i of matrix m
int[] row(int[][] m, int i) {
  assert 0 <= i && i < rows(m);
  return m[i];
}

// column j of matrix m
int[] column(int[][] m, int j) {
  assert 0 <= j && j < columns(m);
  int[] c = new int[rows(m)];
  for (int i = 0; i < rows(m); i++)
    c[i] = m[i][j];
  return c;
}

// scalar product of v and w
int vector_mul(int[] v, int[] w) {
  assert v.length == w.length;
  int res = 0;
  for (int i = 0; i < v.length; i++)
    res = res + v[i] * w[i];
  return res;
}

// matrix multiplication a*b
int[][] matrix_mul(int[][] a, int[][] b) {
  assert columns(a) == rows(b);
  int[][] m = new int[rows(a)][columns(b)];
  for (int i = 0; i < rows(a); i++)
    for (int j = 0; j < columns(b); j++)
      m[i][j] = vector_mul(row(a, i), column(b, j));
  return m;
}

// matrix exponentiation a^n
int[][] matrix_pow(int[][] a, int n) {
  assert n >= 0;
  if (n == 0) return identity(2);
  else if (n % 2 == 0) {
    int[][] b = matrix_pow(a, n / 2);
    return matrix_mul(b, b);
  } else return matrix_mul(a, matrix_pow(a, n - 1));
}

// efficient version, logarithmic
int efficient_fibo(int n) {
  assert n >= 0;
  int[][] a = { { 1, 1 }
              , { 1, 0 } };
  int[][] b = matrix_pow(a, n);
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

test(45);
