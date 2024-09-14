package openrv300.pipeline

import openrv300.pipeline.payload.ExecMemPayload
import openrv300.regfile.GPRsWritePort
import spinal.core._
import spinal.lib._

case class WriteBackGPRs() extends Component {
  val io = new Bundle {
    val request = slave(Flow(ExecMemPayload()))
    val regWritePort = master(GPRsWritePort())
  }

  val reqData = io.request.payload

  io.regWritePort.writeEnable := False
  io.regWritePort.writeAddr := U"5'd0"
  io.regWritePort.writeData := B"32'd0"

  when(io.request.valid && reqData.writeRegDest) {
    io.regWritePort.writeEnable := True
    io.regWritePort.writeAddr := reqData.regDest
    io.regWritePort.writeData := reqData.regDestValue
  }
}
