void exception_handler() __attribute__((section(".trap_handler"), aligned(32)));

void exception_handler() {
    int sum = 0;
    for(int i = 1; i <= 5; i++) {
        sum += i;
    }
    asm volatile (
        "csrrw x29, 0x341, x0\n"   /* mepc */
        "addi  x29, x29, 4\n"
        "csrrw x0, 0x341, x29\n"
        "mret\n"
    );
}

int main() {
    unsigned long trap_handler = 0x80003000;

    asm volatile (
        "csrrw  x0, 0x305, %0\n"  /* mtvec */
        :"=r"(trap_handler)
    );

    volatile int *a = (volatile int*)0x30000000;
    *a = 3;
    return 0;
}
