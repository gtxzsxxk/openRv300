package openrv300.pipeline.control

import spinal.core._
import spinal.lib._

case class BypassUnit() extends Component {
  val io = new Bundle {
    /* exec, mem, wb */
    val writePort = Vec.fill(3)(slave(BypassWritePort()))
    val execReadPorts = Vec.fill(2)(slave(BypassReadPort()))
  }

  val ctx = WhenBuilder()

  /* 旁路网络 */
  for (rdIndex <- 0 until 2) {
    io.execReadPorts(rdIndex).regValue := B"32'd0"
    io.execCheckPorts(rdIndex).pending := False

    when(io.execReadPorts(rdIndex).readEnable) {
      /* 答案已计算完成 */
      /* TODO: 检查这里的优先级 */
      for (idx <- 0 until 1) {
        ctx.when(io.writePort(idx).whichReg === io.execReadPorts(rdIndex).whichReg) {
          io.execReadPorts(rdIndex).regValue := io.writePort(idx).regValue
        }
      }
      for (idx <- 1 until 3) {
        ctx.elsewhen(io.writePort(idx).whichReg === io.execReadPorts(rdIndex).whichReg) {
          io.execReadPorts(rdIndex).regValue := io.writePort(idx).regValue
        }
      }
    }
  }
}
