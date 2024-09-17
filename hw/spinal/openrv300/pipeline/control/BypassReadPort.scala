package openrv300.pipeline.control

import spinal.core._
import spinal.lib._

case class BypassReadPort() extends Bundle with IMasterSlave {
  val whichReg = UInt(5 bits)
  val pending = Bool()
  val isBypassing = Bool()
  val regValue = Bits(32 bits)

  override def asMaster(): Unit = {
    out(whichReg)
    in(regValue, pending, isBypassing)
  }
}
