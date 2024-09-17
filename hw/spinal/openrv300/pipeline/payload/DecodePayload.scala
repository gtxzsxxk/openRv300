package openrv300.pipeline.payload

import spinal.core._

case class DecodePayload() extends Bundle {

  val microOp = Bits(7 bits)

  val instPc = UInt(32 bits)
  val instruction = Bits(32 bits)

  val function0 = Bits(3 bits)
  val function1 = Bits(7 bits)
  val regDest = UInt(5 bits)

  val imm = Bits(20 bits)
  val sextImm = SInt(32 bits)
}
