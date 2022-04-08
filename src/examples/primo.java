int primo(int n) {
    int i = 2;
    while (i < n) {
	if (n % i == 0) return 0;
	i = i + 1;
    }
    if (n >= 2) return 1;
    else return 0;
}
