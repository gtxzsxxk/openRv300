package openrv300.isa

import spinal.core._

object RV32I {
  private val imm20 = "-".repeat(20)
  private val imm12 = "-".repeat(12)
  private val imm7 = "-".repeat(7)
  private val imm5 = "-".repeat(5)
  private val reg = "-".repeat(5)

  val LUI = MaskedLiteral(imm20 + reg + "0110111")
  val AUIPC = MaskedLiteral(imm20 + reg + "0010111")

  val JAL = MaskedLiteral(imm20 + reg + "1101111")
  val JALR = MaskedLiteral(imm12 + reg + "000" + reg + "1100111")

  val BEQ = MaskedLiteral(imm7 + reg + reg + "000" + imm5 + "1100011")
  val BNE = MaskedLiteral(imm7 + reg + reg + "001" + imm5 + "1100011")
  val BLT = MaskedLiteral(imm7 + reg + reg + "100" + imm5 + "1100011")
  val BGE = MaskedLiteral(imm7 + reg + reg + "101" + imm5 + "1100011")
  val BLTU = MaskedLiteral(imm7 + reg + reg + "110" + imm5 + "1100011")
  val BGEU = MaskedLiteral(imm7 + reg + reg + "111" + imm5 + "1100011")

  val LB = MaskedLiteral(imm12 + reg + "000" + reg + "0000011")
  val LH = MaskedLiteral(imm12 + reg + "001" + reg + "0000011")
  val LW = MaskedLiteral(imm12 + reg + "010" + reg + "0000011")
  val LBU = MaskedLiteral(imm12 + reg + "100" + reg + "0000011")
  val LHU = MaskedLiteral(imm12 + reg + "101" + reg + "0000011")

  val SB = MaskedLiteral(imm7 + reg + reg + "000" + imm5 + "0100011")
  val SH = MaskedLiteral(imm7 + reg + reg + "001" + imm5 + "0100011")
  val SW = MaskedLiteral(imm7 + reg + reg + "010" + imm5 + "0100011")

  val ADDI = MaskedLiteral(imm12 + reg + "000" + reg + "0010011")
  val SLTI = MaskedLiteral(imm12 + reg + "010" + reg + "0010011")
  val SLTIU = MaskedLiteral(imm12 + reg + "011" + reg + "0010011")
  val XORI = MaskedLiteral(imm12 + reg + "100" + reg + "0010011")
  val ORI = MaskedLiteral(imm12 + reg + "110" + reg + "0010011")
  val ANDI = MaskedLiteral(imm12 + reg + "111" + reg + "0010011")

  val SLLI = MaskedLiteral("0000000" + reg + reg + "001" + reg + "0010011")
  val SRLI = MaskedLiteral("0000000" + reg + reg + "101" + reg + "0010011")
  val SRAI = MaskedLiteral("0100000" + reg + reg + "101" + reg + "0010011")

  val ADD = MaskedLiteral("0000000" + reg + reg + "000" + reg + "0110011")
  val SUB = MaskedLiteral("0100000" + reg + reg + "000" + reg + "0110011")
  val SLL = MaskedLiteral("0000000" + reg + reg + "001" + reg + "0110011")
  val SLT = MaskedLiteral("0000000" + reg + reg + "010" + reg + "0110011")
  val SLTU = MaskedLiteral("0000000" + reg + reg + "011" + reg + "0110011")
  val XOR = MaskedLiteral("0000000" + reg + reg + "100" + reg + "0110011")
  val SRL = MaskedLiteral("0000000" + reg + reg + "101" + reg + "0110011")
  val SRA = MaskedLiteral("0100000" + reg + reg + "101" + reg + "0110011")
  val OR = MaskedLiteral("0000000" + reg + reg + "110" + reg + "0110011")
  val AND = MaskedLiteral("0000000" + reg + reg + "111" + reg + "0110011")

  val FENCE = MaskedLiteral(imm12 + reg + "000" + reg + "0001111")
  val FENCE_TSO = MaskedLiteral("1000_0011_0011_00000_000_00000_0001111")
  val PAUSE = MaskedLiteral("0000_0001_0000_00000_000_00000_0001111")
  val ECALL = MaskedLiteral("000000000000_00000_000_00000_1110011")
  val EBREAK = MaskedLiteral("000000000001_00000_000_00000_1110011")
}
