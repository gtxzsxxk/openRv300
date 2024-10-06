package openrv300.pipeline.payload

import openrv300.privilege.ThrowTrapRequest
import spinal.core._

case class FetchPayload() extends Bundle {
  val pcAddr = UInt(32 bits)
  val instruction = Bits(32 bits)

  val trap = ThrowTrapRequest()
}
