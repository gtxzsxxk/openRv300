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

  for (rdIndex <- 0 until 2) {
    io.execReadPorts(rdIndex).regValue := B"32'd0"
    io.execReadPorts(rdIndex).pending := False
    io.execReadPorts(rdIndex).isBypassing := False
  }

  /* 旁路网络 */
  for (rdIndex <- 0 until 2) {
    /* TODO: 检查这里的优先级 */
    for (idx <- 0 until 1) {
      ctx.when(io.writePort(idx).whichReg === io.execReadPorts(rdIndex).whichReg && io.execReadPorts(rdIndex).whichReg =/= U"5'd0") {
        io.execReadPorts(rdIndex).regValue := io.writePort(idx).regValue
        io.execReadPorts(rdIndex).pending := !io.writePort(idx).finished
        io.execReadPorts(rdIndex).isBypassing := True
      }
    }
    for (idx <- 1 until 3) {
      ctx.elsewhen(io.writePort(idx).whichReg === io.execReadPorts(rdIndex).whichReg && io.execReadPorts(rdIndex).whichReg =/= U"5'd0") {
        io.execReadPorts(rdIndex).regValue := io.writePort(idx).regValue
        io.execReadPorts(rdIndex).pending := !io.writePort(idx).finished
        io.execReadPorts(rdIndex).isBypassing := True
      }
    }

  }
}
