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

int recursive_fibo(int n) {
  if (n <= 1) return n;
  else return recursive_fibo(n - 1) + recursive_fibo(n - 2);
}

for (int i = 0; i < 40; i++)
  println(recursive_fibo(i) + " " + iterative_fibo(i));