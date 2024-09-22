package openrv300.cache

import spinal.core._
import spinal.lib._

case class CacheLine() extends Bundle {
  /* Virtually-Indexed Physically-Tagged */
  val tag = UInt(20 bits)

  val data = Vec.fill(16)(Bits(32 bits))
}
