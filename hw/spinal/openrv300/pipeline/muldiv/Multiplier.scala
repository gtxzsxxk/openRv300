package openrv300.pipeline.muldiv

import spinal.core._
import spinal.lib._

case class Multiplier() extends BlackBox {
  val io = new Bundle {
    val clk = in port Bool()
    val resetn = in port Bool()
    val A = in port UInt(32 bits)
    val B = in port UInt(32 bits)
    val P = out port UInt(64 bits)
  }

  mapCurrentClockDomain(io.clk, io.resetn, resetActiveLevel = LOW)
  noIoPrefix()
  addRTLPath("hw/spinal/openrv300/pipeline/muldiv/sim/Multiplier.v")
}
