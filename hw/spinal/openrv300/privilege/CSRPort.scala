package openrv300.privilege

import spinal.core._
import spinal.lib._

case class CSRPort() extends Bundle with IMasterSlave {
  val address = UInt(12 bits)
  val readData = Bits(32 bits)
  val writeData = Bits(32 bits)
  val withWrite = Bool()
  val noRead = Bool()
  val valid = Bool()

  override def asMaster(): Unit = {
    out(address, writeData, withWrite, valid, noRead)
    in(readData)
  }
}
