// Calcolo del minimo comune multiplo

int mcm(int a, int b) {
    int m = a, n = b;
    while (m != n) {
        if (m < n) m = m + a;
        else n = n + b;
    }
    return m;
}

for (int i = 1; i < 10; i++)
  for (int j = i; j < 10; j++)
    println("mcm(" + i + "," + j + ") = " + mcm(i,j));
  