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

boolean prime(int n) {
    assert n > 0 : "prime";
    for (int i = 2; i < n; i++)
        if (n % i == 0) return false;
    return n != 1;
}

int x;
do {
    x = getInt("Give me a number (0 to stop)");
    if (x > 0) println(x + " is " + (prime(x) ? "" : "not ") + "prime");
} while (x > 0);
