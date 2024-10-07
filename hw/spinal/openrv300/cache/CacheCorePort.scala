package openrv300.cache

import spinal.core._
import spinal.lib._

case class CacheCorePort() extends Bundle with IMasterSlave {
  val address = UInt(32 bits)
  val valid = Bool()

  val isWrite = Bool()

  val readValue = Bits(32 bits)
  val writeValue = Bits(32 bits)
  val writeMask = Bits(4 bits)

  val needStall = Bool()

  val fault = Bool()

  override def asMaster(): Unit = {
    out(address, isWrite, writeValue, valid, writeMask)
    in(readValue, needStall, fault)
  }
}
