package openrv300.isa

import spinal.core._

object MicroOp {
  val LUI = U"7'd1"
  val AUIPC = U"7'd2"
  val JAL = U"7'd3"
  val JALR = U"7'd4"
  val BRANCH = U"7'd5"
  val LOAD = U"7'd6"
  val STORE = U"7'd7"
  val ARITH_BINARY_IMM = U"7'd8"
  val ARITH_SHIFT_IMM = U"7'd9"
  val ARITH_BINARY = U"7'd10"
  val ARITH_SHIFT = U"7'd11"
  val ENV = U"7'd12"
}
