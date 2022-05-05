
int mcm(int a, int b) {
    int m = a, n = b;
    while (m != n) {
        if (m < n) m = m + a;
        else n = n + b;
    }
    return m;
}

for (int i = 1;; i < 10; i++;)
  for (int j = i;; j < 10; j++;) {
    print(int_to_string(i));
    print(" ");
    print(int_to_string(j));
    print(" ~> ");
    println(int_to_string(mcm(i, j)));
  }