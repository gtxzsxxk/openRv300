package openrv300.pipeline.muldiv

import spinal.core._
import spinal.lib._

case class Divider() extends BlackBox {
  val io = new Bundle {
    val aclk = in port Bool()
    val s_axis_divisor_tdata = in port UInt(32 bits)
    val s_axis_dividend_tdata = in port UInt(32 bits)
    val m_axis_dout_tdata = out port UInt(64 bits)

    val s_axis_divisor_tvalid = in port Bool()
    val s_axis_dividend_tvalid = in port Bool()
    val m_axis_dout_tvalid = out port Bool()
  }

  mapCurrentClockDomain(io.aclk)
  noIoPrefix()
  addRTLPath("hw/spinal/openrv300/pipeline/muldiv/sim/Divider.v")
}
