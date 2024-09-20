package openrv300.tests

import openrv300.ddr._
import openrv300.Config
import openrv300.Config.axiConfig
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._

object CrossbarTest extends App {

  case class CrossbarTestTop() extends Component {
    val io = new Bundle {
      val iBus = slave(Axi4(axiConfig))
      val dBus = slave(Axi4(axiConfig))
    }

    val ddrSim = DDRSim()
    val crossbar = InstDataCrossbar()
    ddrSim.io.memPort <> crossbar.io.coreBus
    io.iBus <> crossbar.io.iBus
    io.dBus <> crossbar.io.dBus
  }

  Config.sim.compile(CrossbarTestTop()).doSim { dut =>
    val writeData: Array[Long] = Array.fill(16)(scala.util.Random.nextInt() & 0xFFFFFFFFL)
    val addr = scala.util.Random.nextInt() & 0xFFCL

    /* use iBus to write */
    {
      val aw = dut.io.iBus.aw
      val w = dut.io.iBus.w
      val b = dut.io.iBus.b
      aw.valid #= false
      w.valid #= false
      b.ready #= false
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
    }

    dut.clockDomain.waitRisingEdge()

    /* use dBus to read */
    {
      val ar = dut.io.dBus.ar
      val r = dut.io.dBus.r
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
    }
  }
}
