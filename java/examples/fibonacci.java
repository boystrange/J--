int fibo(int n) {
    if (n <= 1) return n;
    else return fibo(n - 1) + fibo(n - 2);
}

int fibo(int k) {
    int m = 0;
    int n = 1;
    while (k >= 0) {
	int t = m;
	m = n;
	n = m + n;
	k = k - 1;
    }
    return m;
}
