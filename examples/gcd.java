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

// Compute greatest common divisor

int recursive_gcd(int a, int b) {
  assert a > 0 && b > 0;
  int r = a % b;
  return r == 0 ? b : recursive_gcd(b, r);
}

int iterative_gcd(int a, int b) {
  assert a > 0 && b > 0;
  int r;
  do {
    r = a % b;
    if (r != 0) {
      a = b;
      b = r;
    }
  } while (r != 0);
  return b;
}

for (int i = 1; i < 100; i++)
  for (int j = 1; j < i; j++)
    println("gcd(" + i + "," + j + ") = " + iterative_gcd(i, j) + " " + recursive_gcd(i, j));
  
