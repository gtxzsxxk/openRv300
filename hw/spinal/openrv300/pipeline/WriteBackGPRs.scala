package openrv300.pipeline

import openrv300.pipeline.control.BypassWritePort
import openrv300.pipeline.payload.ExecMemPayload
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
  }

  val reqData = io.request.payload

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

  def insertBypass(): Unit = {
    bypassWPort.whichReg := reqData.regDest
    bypassWPort.regValue := reqData.regDestValue
    bypassWPort.finished := True
  }

  when(io.request.valid && reqData.writeRegDest) {
    io.regWritePort.writeEnable := True
    io.regWritePort.writeAddr := reqData.regDest
    io.regWritePort.writeData := reqData.regDestValue

    insertBypass()
  }
}
