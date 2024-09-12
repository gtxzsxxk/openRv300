package openrv300

import spinal.core._

case class GPRs() extends Component {
  val io = new Bundle{
    val readAddr0 = in port UInt(5 bits)
    val readData0 = out port UInt(32 bits)
    val readEnable0 = in port Bool()

    val readAddr1 = in port UInt(5 bits)
    val readData1 = out port UInt(32 bits)
    val readEnable1 = in port Bool()
  }

  /* TODO: 手动设置为0，可以先在仿真的时候加上 */
  val registers = Mem(Bits(32 bits), wordCount = 32)

  when(io.readEnable0) {
    io.readData0 := registers(io.readAddr0).asUInt
  }

  when(io.readEnable1) {
    io.readData1 := registers(io.readAddr1).asUInt
  }
}
