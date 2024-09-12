package openrv300.isa

import spinal.core.MaskedLiteral

object RV32A {
  private val reg = "-".repeat(5)
  private val aq_rl = "--"

  val LR_W = MaskedLiteral("00010" + aq_rl + "00000" + reg + "010" + reg + "0101111")
  val SC_W = MaskedLiteral("00011" + aq_rl + reg + reg + "010" + reg + "0101111")
  val AMOSWAP_W = MaskedLiteral("00001" + aq_rl + reg + reg + "010" + reg + "0101111")
  val AMOADD_W = MaskedLiteral("00000" + aq_rl + reg + reg + "010" + reg + "0101111")
  val AMOXOR_W = MaskedLiteral("00100" + aq_rl + reg + reg + "010" + reg + "0101111")
  val AMOAND_W = MaskedLiteral("01100" + aq_rl + reg + reg + "010" + reg + "0101111")
  val AMOOR_W = MaskedLiteral("01000" + aq_rl + reg + reg + "010" + reg + "0101111")
  val AMOMIN_W = MaskedLiteral("10000" + aq_rl + reg + reg + "010" + reg + "0101111")
  val AMOMAX_W = MaskedLiteral("10100" + aq_rl + reg + reg + "010" + reg + "0101111")
  val AMOMINU_W = MaskedLiteral("11000" + aq_rl + reg + reg + "010" + reg + "0101111")
  val AMOMAXU_W = MaskedLiteral("11100" + aq_rl + reg + reg + "010" + reg + "0101111")
}
