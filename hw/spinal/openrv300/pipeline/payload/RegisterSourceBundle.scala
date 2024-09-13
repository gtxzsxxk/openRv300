package openrv300.pipeline.payload

import spinal.core._

case class RegisterSourceBundle() extends Bundle {
  val which = UInt(5 bits)
  val value = Bits(32 bits)
}
