.global _boot
.text

    li  a0, 0
    li  a1, 1
    li  t0, 101
loop:
    add a0, a0, a1
    addi    a1, a1, 1
    blt a1, t0, loop

    ecall
