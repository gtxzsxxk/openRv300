package openrv300.pipeline.control

import spinal.core._
import spinal.lib._

case class BypassWritePort() extends Bundle with IMasterSlave {
  val whichReg = UInt(5 bits)
  /* 答案已计算完成 */
  val finished = Bool()
  val regValue = Bits(32 bits)

  override def asMaster(): Unit = {
    out(whichReg, finished, regValue)
  }
}
