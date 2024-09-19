.text
    /* x1=0x12345678, x2=0xfafafafa, x3=0x4, x4=0x5678fafa */
    li  x1, 0x12345678
    li  x2, 0xfbfbfafa
    li  x3, 0x4
    sh  x2, 0(x3)
    sh  x1, 2(x3)
    lw  x4, 0(x3)
