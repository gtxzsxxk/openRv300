int main() {
    volatile int *a = 0x30000000;
    *a = 3;
    return 0;
}