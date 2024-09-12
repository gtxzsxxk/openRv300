package openrv300.pipeline.payload

import spinal.core._

case class FetchPayload() extends Bundle {
  val pcAddr = UInt(32 bits)
  val instruction = UInt(32 bits)
}
