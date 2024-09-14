package openrv300.pipeline.control

import spinal.core._
import spinal.lib._

case class BypassWritePort() extends Bundle with IMasterSlave {
  /* 记录寄存器值经过了此阶段 */
  val passed = Bool()
  val whichReg = UInt(5 bits)
  /* 答案已计算完成 */
  val finished = Bool()
  val regValue = Bits(32 bits)

  override def asMaster(): Unit = {
    out(whichReg, finished, passed, regValue)
  }
}
