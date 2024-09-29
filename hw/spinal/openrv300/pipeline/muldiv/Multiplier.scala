package openrv300.pipeline.muldiv

import spinal.core._
import spinal.lib._

case class Multiplier() extends BlackBox {
  val io = new Bundle {
    val A = in port UInt(32 bits)
    val B = in port UInt(32 bits)
    val P = in port UInt(64 bits)
  }
  mapCurrentClockDomain(io.clk)
  noIoPrefix()
  addRTLPath("multiplier_sim.v")
}
