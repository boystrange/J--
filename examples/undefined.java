
int iterative_fibo(int n) {
  int a = 0, b = 1;
  while (n > 0) {
    int c = a + b;
    a = b;
    b = c;
    n--;
  }
  return a;
}

int recursive_fibo(int n) {
  if (n <= 1) return n;
  else return recursive_fibo(n - 1) + recursive_fibo(n - 2);
}

int recursive_fact(int n) {
  if (n == 0) return 1;
  else return n * recursive_fact(n - 1);
}

println(int_to_string(recursive_fibo(10)));
println(int_to_string(recursive_fact(10)));
println(int_to_string(iterative_fibo(10)));