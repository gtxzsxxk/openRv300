package openrv300.pipeline.control

import spinal.core._
import spinal.lib._

case class BypassUnit() extends Component {
  val io = new Bundle {
    /* exec, mem, wb */
    val writePort = Vec.fill(3)(slave(BypassWritePort()))
    val execReadPorts = Vec.fill(2)(slave(BypassReadPort()))
  }

  for (rdIndex <- 0 until 2) {
    io.execReadPorts(rdIndex).regValue := B"32'd0"
    io.execReadPorts(rdIndex).pending := False
    io.execReadPorts(rdIndex).isBypassing := False
  }

  /* 旁路网络 */
  for (rdIndex <- 0 until 2) {
    when(io.writePort(0).whichReg === io.execReadPorts(rdIndex).whichReg && io.execReadPorts(rdIndex).whichReg =/= U"5'd0") {
      io.execReadPorts(rdIndex).regValue := io.writePort(0).regValue
      io.execReadPorts(rdIndex).pending := !io.writePort(0).finished
      io.execReadPorts(rdIndex).isBypassing := True
    } elsewhen (io.writePort(1).whichReg === io.execReadPorts(rdIndex).whichReg && io.execReadPorts(rdIndex).whichReg =/= U"5'd0") {
      io.execReadPorts(rdIndex).regValue := io.writePort(1).regValue
      io.execReadPorts(rdIndex).pending := !io.writePort(1).finished
      io.execReadPorts(rdIndex).isBypassing := True
    } elsewhen (io.writePort(2).whichReg === io.execReadPorts(rdIndex).whichReg && io.execReadPorts(rdIndex).whichReg =/= U"5'd0") {
      io.execReadPorts(rdIndex).regValue := io.writePort(2).regValue
      io.execReadPorts(rdIndex).pending := !io.writePort(2).finished
      io.execReadPorts(rdIndex).isBypassing := True
    }
  }
}
