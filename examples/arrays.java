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

boolean sorted(int[] a) {
    for (int i = 0; i < a.length - 1; i++)
        if (a[i] > a[i + 1]) return false;
    return true;
}

assert sorted(new int[] { })         : "empty is sorted" ;
assert sorted(new int[] { 1 })       : "singleton is sorted";
assert sorted(new int[] { 1, 2 })    : "sorted";
assert sorted(new int[] { 1, 2, 3 }) : "sorted";
assert !sorted(new int[] { 2, 1 })   : "not sorted";