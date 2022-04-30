boolean even(int n) {
    return n == 0 || odd(n - 1);
}

boolean odd(int n) {
    return even(n - 1);
}
