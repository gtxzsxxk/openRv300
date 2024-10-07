package openrv300.pipeline

import openrv300.pipeline.control.BypassWritePort
import openrv300.pipeline.payload.ExecMemPayload
import openrv300.privilege.ThrowTrapRequest
import openrv300.regfile.GPRsWritePort
import spinal.core._
import spinal.lib._

case class WriteBackGPRs() extends Component {
  val io = new Bundle {
    val request = slave(Flow(ExecMemPayload()))
    val regWritePort = master(GPRsWritePort())
    /* 与寄存器堆的读取分开，寄存器堆的读取仅在decode阶段 */
    /* 所以这里新写入的值，会通过旁路网络，下一拍decode才可以从寄存器读到最新的值 */
    val bypassWritePort = master(BypassWritePort())

    val throwTrapPort = master(ThrowTrapRequest())
  }

  val reqData = io.request.payload
  val reqValid = io.request.valid

  io.regWritePort.writeEnable := False
  io.regWritePort.writeAddr := U"5'd0"
  io.regWritePort.writeData := B"32'd0"

  val bypassWPort = Reg(BypassWritePort())
  bypassWPort.finished init (False)
  bypassWPort.whichReg init (U"5'd0")
  bypassWPort.regValue init (B"32'd0")
  io.bypassWritePort := bypassWPort
  bypassWPort.whichReg := U"5'd0"
  bypassWPort.finished := False
  bypassWPort.regValue := B"32'd0"

  def insertBypass(solvedThisStage: Boolean): Unit = {
    bypassWPort.whichReg := reqData.regDest
    bypassWPort.regValue := reqData.regDestValue
    bypassWPort.finished := Bool(solvedThisStage)
  }

  when(reqValid && reqData.writeRegDest && !reqData.trap.throwTrap) {
    io.regWritePort.writeEnable := True
    io.regWritePort.writeAddr := reqData.regDest
    io.regWritePort.writeData := reqData.regDestValue

    insertBypass(true)
  }

  io.throwTrapPort.throwTrap := False
  io.throwTrapPort.trapCause := 0
  io.throwTrapPort.trapPc := 0
  io.throwTrapPort.trapValue := 0
  /* 由于指令引起了异常，因此这条指令不能被标记为valid， */
  /* 否则相当于提交了一条引起异常的指令。 */
  when(reqData.trap.throwTrap) {
    io.throwTrapPort := reqData.trap
  }
}
