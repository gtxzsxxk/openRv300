.global _boot
.text

_boot:
    /* x1 = 0x4 */
    /* x2 = 0x6 */
    /* x3 = 0xB */
    /* x4 = 0xD */
    addi    x1, x0, 0x4
    addi    x2, x0, 0x6
    addi    x3, x1, 0x7
    sw      x3, 0(x1)
    sw      x3, 0(x3)
    lw      x4, 0(x1)
    addi    x4, x4, 0x01
    addi    x4, x4, 0x01

.data
variable:
	.word 0xdeadbeef
                    