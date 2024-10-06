package openrv300.privilege

import spinal.core._
import spinal.lib._

case class DoTrapRequest() extends Bundle with IMasterSlave {
  val trapJumpAddress = UInt(32 bits)
  val trapValid = Bool()
  val trapReady = Bool()

  override def asMaster(): Unit = {
    out(trapJumpAddress, trapValid)
    in(trapReady)
  }
}
