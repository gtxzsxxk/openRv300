package openrv300.regfile

import spinal.core._
import spinal.lib.IMasterSlave

case class GPRsWritePort() extends Bundle with IMasterSlave {
  val writeAddr = UInt(5 bits)
  val writeData = Bits(32 bits)
  val writeEnable = Bool()

  override def asMaster(): Unit = {
    out(writeAddr, writeData, writeEnable)
  }
}
