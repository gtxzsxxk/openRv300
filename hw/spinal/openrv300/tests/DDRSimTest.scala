package openrv300.tests

import openrv300.Config
import openrv300.ddr.DDRSim
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba4.axi._

import scala.util.control.Breaks.break

object DDRSimTest extends App {
  Config.sim.compile {
    val ddr = DDRSim()
    ddr.simMemory.simPublic()
    ddr
  }.doSim { dut =>
    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.fallingEdge()
    dut.clockDomain.assertReset()
    dut.clockDomain.waitRisingEdge()
    dut.clockDomain.deassertReset()

    val aw = dut.io.memPort.aw
    val w = dut.io.memPort.w
    val b = dut.io.memPort.b
    val ar = dut.io.memPort.ar
    val r = dut.io.memPort.r

    aw.valid #= false
    w.valid #= false
    b.ready #= false
    ar.valid #= false
    r.ready #= false

    /* 写测试 */
    val writeData: Array[Long] = Array.fill(16)(scala.util.Random.nextInt() & 0xFFFFFFFFL)
    val addr = scala.util.Random.nextInt() & 0xFFCL
    aw.payload.id #= 0x2
    aw.payload.addr #= addr
    aw.payload.len #= 64 - 1
    aw.payload.size #= 2
    aw.payload.burst #= 1
    aw.valid #= true
    while (!aw.ready.toBoolean) {
      dut.clockDomain.waitRisingEdge()
    }
    aw.valid #= false
    while (!w.ready.toBoolean) {
      dut.clockDomain.waitRisingEdge()
    }
    for (idx <- 0 until 16) {
      w.payload.data #= writeData(idx)
      w.payload.strb #= 15
      w.payload.last #= idx == 15
      w.valid #= true
      dut.clockDomain.waitRisingEdge()
    }
    w.valid #= false
    b.ready #= true
    while (!b.valid.toBoolean) {
      dut.clockDomain.waitRisingEdge()
    }
    assert(b.payload.resp.toInt == 0, "AXI Response Error")
    b.ready #= false

    for (idx <- 0 until 16) {
      assert(dut.simMemory.getBigInt(addr + idx * 4) == writeData(idx))
    }

    /* 读测试 */
    ar.payload.id #= 0x3
    ar.payload.addr #= addr
    ar.payload.len #= 64 - 1
    ar.payload.size #= 2
    ar.payload.burst #= 1
    ar.valid #= true
    while (!ar.ready.toBoolean) {
      dut.clockDomain.waitRisingEdge()
    }
    ar.valid #= false
    r.ready #= true
    while (!r.valid.toBoolean) {
      dut.clockDomain.waitRisingEdge()
    }
    for (idx <- 0 until 16) {
      assert(r.payload.resp.toInt == 0)
      assert(r.payload.last.toBoolean == (idx == 15))
      assert(r.payload.data.toLong == writeData(idx))
      dut.clockDomain.waitRisingEdge()
    }
    r.ready #= false
    dut.clockDomain.waitRisingEdge()
  }
}
