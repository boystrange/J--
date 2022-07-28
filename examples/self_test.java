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

void check_assignement() {
    int a = 0;
    int b = 0;
    a = b = 1;
    assert a == b;
    assert a == 1;
}

void check_relations() {
    assert 1 < 2;
    assert 2 <= 2;
    assert 2 > 1;
    assert 2 >= 1;
    assert 2 == 2;
    assert 1 != 2;
    assert true == true;
    assert false == false;
    assert true != false;
}

void check_unary_operators() {
    int a = 1;
    assert +a == a;
    assert -a == 0 - a;
    assert -(-a) == a;
    assert -(-(-a)) == -a;
}

void check_step_operators() {
    int a = 0;
    assert a++ == 0;
    assert a++ == 1;
    assert ++a == 3;
    assert --a == 2;
    assert a-- == 2;
    int b = a = 0;
    assert a++ < ++b;
    assert a == b;
}

void check_arrays() {
    int[] a = { 1, 2, 3 };
    assert a.length == 3;
    assert a[0] == 1;
    assert a[1] == 2;
    a[0] = 0;
    assert a[0] == 0;
    assert a[0]++ == 0;
    assert ++a[0] == 2;
    assert --a[0] == 1;
}

void check_bidimensional_arrays() {
    int[][] a = { { 1, 2, 3 }, { 4, 5, 6 } };
    assert a.length == 2;
    assert a[0].length == 3;
    assert a[1].length == 3;
    assert a[0][0] == 1;
    a[0][0] = 0;
    assert a[0][0] == 0;
    assert a[0][0]++ == 0;
    assert ++a[0][0] == 2;
    assert --a[0][0] == 1;
}

void check_short_circuit() {
    assert 1 < 2 || 1 / 0 == 0;
    assert !(2 < 1 && 1 / 0 == 0);
    assert !(!(1 < 2) && 1 / 0 == 0);
}

void check_characters() {
    char c = (char) ('a' + 1);
    println("after 'a' we have '" + c + "' and then '" + (char) (c + 1) + "'");
    println("upper case of 'a' is '" + to_upper('a') + "'");
}

char to_upper(char c) {
    return c >= 'a' && c <= 'z' ? (char) (c - 'a' + 'A') : c;
}

void check_unicode_escape() {
    println("Print a greek alpha \u03B1");
}

check_assignement();
check_relations();
check_unary_operators();
check_step_operators();
check_arrays();
check_bidimensional_arrays();
check_short_circuit();
check_characters();
check_unicode_escape();
