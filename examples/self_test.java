
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
    assert a[0] == 0 : "array assignment";
    assert a[0]++ == 0 : "array post increment";
    assert ++a[0] == 2 : "array pre increment";
}

check_step_operators();
check_arrays();