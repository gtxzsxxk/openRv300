package openrv300.isa

import spinal.core.MaskedLiteral

object ZICSR_ZIFENCEI {
  private val imm12 = "-".repeat(12)
  private val reg = "-".repeat(5)

  val FENCE_I = MaskedLiteral(imm12 + reg + "001" + reg + "0001111")

  val CSRRW = MaskedLiteral(imm12 + reg + "001" + reg + "1110011")
  val CSRRS = MaskedLiteral(imm12 + reg + "010" + reg + "1110011")
  val CSRRC = MaskedLiteral(imm12 + reg + "011" + reg + "1110011")
  val CSRRWI = MaskedLiteral(imm12 + reg + "101" + reg + "1110011")
  val CSRRSI = MaskedLiteral(imm12 + reg + "110" + reg + "1110011")
  val CSRRCI = MaskedLiteral(imm12 + reg + "111" + reg + "1110011")
}
