package openrv300.pipeline.control

import spinal.core._
import spinal.lib._

case class BypassCheckPort() extends Bundle with IMasterSlave {
  val whichReg = UInt(5 bits)
  val checkEnable = Bool()
  val pending = Bool()

  override def asMaster(): Unit = {
    out(whichReg, checkEnable)
    in(pending)
  }
}
