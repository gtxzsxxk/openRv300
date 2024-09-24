.global _boot
.text

_boot:
    /* x1 = 0x80001000 */
    /* x2 = 0x6 */
    /* x3 = 0x6 */
    /* x4 = 0x8 */
    li      x1, 0x80001000
    addi    x2, x0, 0x6
    sw      x2, 0(x1)
    sw      x2, 4(x1)
    lw      x3, 0(x1)
    lw      x4, 4(x1)
    addi    x4, x4, 0x01
    addi    x4, x4, 0x01

.data
variable:
	.word 0xdeadbeef
                    