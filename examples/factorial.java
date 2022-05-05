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

int iterative_fact(int n) {
  int r = 1;
  while (n > 0) {
    r = r * n;
    n--;
  }
  return r;
}

int recursive_fact(int n) {
  if (n == 0) return 1;
  else return n * recursive_fact(n - 1);
}

for (int i = 0; i < 14; i++)
  println(i + "! = " + recursive_fact(i) + " = " + iterative_fact(i));
