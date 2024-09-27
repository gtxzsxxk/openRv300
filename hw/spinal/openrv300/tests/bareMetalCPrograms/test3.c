int fib(int num) {
    if(num == 1 || num == 2) {
        return 1;
    }
    return fib(num - 1) + fib(num - 2);
}

int main() {
    int d = 10;
    return fib(10);
}
