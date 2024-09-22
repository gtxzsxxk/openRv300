package openrv300.cache

import spinal.core._
import spinal.lib._

case class CacheLineFlags(ways: Int) extends Bundle {
  val validVec = Vec.fill(ways)(Bool())
  val dirtyVec = Vec.fill(ways)(Bool())
  val counterVec = Vec.fill(ways)(UInt(64 bits))
}
