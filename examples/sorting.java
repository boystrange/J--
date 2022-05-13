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

void println_array(int[] a) {
    print("[");
    for (int i = 0; i < a.length; i++)
	print(" " + a[i]);
    println(" ]");
}

void swap(int[] a, int i, int j) {
    assert 0 <= i && i < a.length;
    assert 0 <= j && j < a.length;
    int x = a[i];
    a[i] = a[j];
    a[j] = x;
}

void bubble_sort(int[] a) {
    for (int i = a.length - 1; i >= 0; i--)
	for (int j = 0; j < i; j++)
	    if (a[j] > a[j + 1]) swap(a, j, j + 1);
}

int[] random_array(int n) {
    assert n >= 0;
    int[] a = new int[n];
    for (int i = 0; i < n; i++)
	a[i] = random_int(100);
    return a;
}

int[] a = random_array(20);
println_array(a);
bubble_sort(a);
println_array(a);
