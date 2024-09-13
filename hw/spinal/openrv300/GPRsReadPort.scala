package openrv300

import spinal.core._
import spinal.lib.{IMasterSlave, master}

case class GPRsReadPort() extends Bundle with IMasterSlave {
  val readAddr = UInt(5 bits)
  val readData = Bits(32 bits)
  val readEnable = Bool()

  override def asMaster(): Unit = {
    out(readAddr, readEnable)
    in(readData)
  }
}
