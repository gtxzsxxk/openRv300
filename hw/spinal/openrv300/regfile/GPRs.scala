package openrv300.regfile

import spinal.core._
import spinal.lib._

case class GPRs() extends Component {
  val io = new Bundle {
    val decodePorts = Vec.fill(2)(slave(GPRsReadPort()))

    val writeBackPort = slave(GPRsWritePort())
  }

  /* TODO: 手动设置为0，可以先在仿真的时候加上 */
  val registers = Mem(Bits(32 bits), wordCount = 32)

  for (idx <- 0 until 2) {
    io.decodePorts(idx).readData := B"32'd0"

    when(io.decodePorts(idx).readEnable) {
      when(io.decodePorts(idx).readAddr === U"5'd0") {
        io.decodePorts(idx).readData := B"32'd0"
      } elsewhen (io.writeBackPort.writeEnable && io.writeBackPort.writeAddr === io.decodePorts(idx).readAddr) {
        io.decodePorts(idx).readData := io.writeBackPort.writeData
      } otherwise {
        io.decodePorts(idx).readData := registers(io.decodePorts(idx).readAddr)
      }
    }
  }

  registers.write(io.writeBackPort.writeAddr, Mux(io.writeBackPort.writeAddr =/= U"5'd0", io.writeBackPort.writeData, B"32'd0"), enable = io.writeBackPort.writeEnable)
}
