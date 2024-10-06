package openrv300.privilege

import spinal.core._
import spinal.lib._

case class ThrowTrapRequest() extends Bundle with IMasterSlave {
  val throwTrap = Bool()
  val trapCause = UInt(12 bits)
  val trapPc = UInt(32 bits)
  val trapValue = Bits(32 bits)

  override def asMaster(): Unit = {
    out(throwTrap, trapCause, trapPc, trapValue)
  }
}
