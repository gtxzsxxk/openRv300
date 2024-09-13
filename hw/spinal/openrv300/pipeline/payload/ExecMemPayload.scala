package openrv300.pipeline.payload

import spinal.core._

case class ExecMemPayload() extends Bundle {
  val microOp = Bits(7 bits)

  val instPc = UInt(32 bits)
  val instruction = Bits(32 bits)

  val function0 = Bits(3 bits)
  val function1 = Bits(7 bits)

  val regSource0 = RegisterSourceBundle()
  val regSource1 = RegisterSourceBundle()
  val regDest = UInt(5 bits)

  val imm = Bits(20 bits)
  val sextImm = SInt(32 bits)

  val writeRegDest = Bool()
  val regDestValue = Bits(32 bits)

  val takeJump = Bool()
  val jumpPc = UInt(32 bits)

  val memoryAddress = UInt(32 bits)
}
