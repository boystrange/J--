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

void check_relations() {
    assert 1 < 2          : "<";
    assert 2 <= 2         : "<=";
    assert 2 > 1          : ">";
    assert 2 >= 1         : ">=";
    assert 2 == 2         : "==";
    assert 1 != 2         : "!=";
    assert true == true   : "==";
    assert false == false : "==";
    assert true != false  : "!=";
}

void check_unary_operators() {
    int a = 1;
    assert +a == a        : "+";
    assert -a == 0 - a    : "-";
    assert -(-a) == a     : "--";
    assert -(-(-a)) == -a : "---";
}

void check_step_operators() {
    int a = 0;
    assert a++ == 0 : "post increment";
    assert a++ == 1 : "post increment";
    assert ++a == 3 : "pre increment";
    assert --a == 2 : "pre decrement";
    assert a-- == 2 : "post decrement";
}

void check_arrays() {
    int[] a = new int[] { 1, 2, 3 };
    assert a.length == 3 : "array length";
    assert a[0] == 1 : "array read";
    assert a[1] == 2 : "array read";
    a[0] = 0;
    assert a[0] == 0 : "array write";
    assert a[0]++ == 0 : "array post increment";
    assert ++a[0] == 2 : "array pre increment";
    assert --a[0] == 1 : "array pre decrement";
}

void check_bidimensional_arrays() {
    int[][] a = new int[][] { { 1, 2, 3 }, { 4, 5, 6 } };
    assert a.length == 2 : "array length";
    assert a[0].length == 3 : "array length";
    assert a[0][0] == 1 : "array read";
    a[0][0] = 0;
    assert a[0][0] == 0 : "array write";
    assert a[0][0]++ == 0 : "array post increment";
    assert ++a[0][0] == 2 : "array pre increment";
    assert --a[0][0] == 1 : "array pre decrement";
}

void check_short_circuit() {
    assert 1 < 2 || 1 / 0 == 0    : "||";
    assert !(2 < 1 && 1 / 0 == 0) : "&&";
    assert !(!(1 < 2) && 1 / 0 == 0) : "!&&";
    // assert 1 < 2 || 1 / 0    : "||"; // IMPROVE ERROR MESSAGE
}

check_relations();
check_unary_operators();
check_step_operators();
check_arrays();
check_bidimensional_arrays();
check_short_circuit();