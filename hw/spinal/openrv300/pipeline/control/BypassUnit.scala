package openrv300.pipeline.control

import spinal.core._
import spinal.lib._

case class BypassUnit() extends Component {
  val io = new Bundle {
    /* exec, mem, wb */
    val writePort = Vec.fill(3)(slave(BypassWritePort()))

    val execReadPort = slave(BypassReadPort())
  }

  io.execReadPort.pending := True
  io.execReadPort.regValue := B"32'd0"

  val ctx = WhenBuilder()

  /* 旁路网络 */
  when(io.execReadPort.readEnable) {
    /* 答案已计算完成 */
    /* TODO: 检查这里的优先级 */
    for (idx <- 0 until 1) {
      ctx.when(io.writePort(idx).finished && io.writePort(idx).whichReg === io.execReadPort.whichReg) {
        io.execReadPort.pending := False
        io.execReadPort.regValue := io.writePort(idx).regValue
      }
    }
    for (idx <- 1 until 3) {
      ctx.elsewhen(io.writePort(idx).finished && io.writePort(idx).whichReg === io.execReadPort.whichReg) {
        io.execReadPort.pending := False
        io.execReadPort.regValue := io.writePort(idx).regValue
      }
    }
  }
}
