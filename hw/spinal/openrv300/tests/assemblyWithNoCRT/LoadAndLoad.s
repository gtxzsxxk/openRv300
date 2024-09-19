.text
    /* x1=0x4, x2=0x8, x3=0x99, x4=0x99 x5=0x9B x6=0x88 */
    addi    x1, x0, 0x4
    addi    x2, x0, 0x8
    addi    x3, x0, 0x99
    addi    x4, x0, 0x87
    sb      x4, 0(x3)
    sw      x3, 0(x2)
    sw      x2, 0(x1)
    lw      x4, 0(x1)
    lw      x4, 0(x4)
    lbu     x6, 0(x4)
    addi    x5, x4, 0x01
    addi    x5, x4, 0x02
    addi    x6, x6, 0x01
