package openrv300.privilege

import spinal.core._
import spinal.lib._

case class ThrowTrapInformation() extends Bundle with IMasterSlave {
  val throwTrap = Bool()
  val trapCause = UInt(12 bits)
  val trapPc = UInt(32 bits)
  val trapValue = Bits(32 bits)
  /* fetch 0 decode 1 exec 2 mem 3 */
  val fromWhichStage = UInt(3 bits)

  override def asMaster(): Unit = {
    out(throwTrap, trapCause, trapPc, trapValue, fromWhichStage)
  }
}
