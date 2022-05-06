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

// Compute least common multiple

int mcm(int a, int b) {
    int m = a, n = b;
    while (m != n) {
        if (m < n) m = m + a;
        else n = n + b;
    }
    return m;
}

for (int i = 1; i < 10; i++)
  for (int j = i; j < 10; j++)
    println("mcm(" + i + "," + j + ") = " + mcm(i,j));
  