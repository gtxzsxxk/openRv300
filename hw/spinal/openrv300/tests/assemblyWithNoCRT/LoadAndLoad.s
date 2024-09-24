.text
    /* x1=0x80000f04, x2=0x80001f04, x3=0x80000f14, x4=0x80001f14 x5=0x9D x6=0x9C */
    li      x1, 0x80000f04
    li      x2, 0x80001f04
    addi    x3, x1, 0x10
    addi    x4, x2, 0x10
    addi    x5, x0, 0x9B
    sb      x5, 0(x4)
    sw      x4, 0(x3)
    sw      x3, 0(x2)
    sw      x2, 0(x1)
    lw      x4, 0(x1)   /* x4 = x2 */
    lw      x4, 0(x4)   /* x4 = x3 */
    lw      x4, 0(x4)   /* x4 = x4 */
    lbu     x6, 0(x4)
    addi    x5, x6, 0x01
    addi    x5, x5, 0x01
    addi    x6, x6, 0x01
