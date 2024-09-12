package openrv300.isa

import spinal.core.MaskedLiteral

object RV32M {
  private val reg = "-".repeat(5)

  val MUL = MaskedLiteral("0000001" + reg + reg + "000" + reg + "0110011")
  val MULH = MaskedLiteral("0000001" + reg + reg + "001" + reg + "0110011")
  val MULHSU = MaskedLiteral("0000001" + reg + reg + "010" + reg + "0110011")
  val MULHU = MaskedLiteral("0000001" + reg + reg + "011" + reg + "0110011")

  val DIV = MaskedLiteral("0000001" + reg + reg + "100" + reg + "0110011")
  val DIVU = MaskedLiteral("0000001" + reg + reg + "101" + reg + "0110011")

  val REM = MaskedLiteral("0000001" + reg + reg + "110" + reg + "0110011")
  val REMU = MaskedLiteral("0000001" + reg + reg + "111" + reg + "0110011")
}
