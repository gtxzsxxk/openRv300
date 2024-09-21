package openrv300.tests

import openrv300.Config
import openrv300.Config.axiConfig
import openrv300.cache._
import openrv300.ddr.DDRSim
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

object CacheTest extends App {
  case class CacheTestTop() extends Component {
    val io = new Bundle {
      val corePort = slave(CacheCorePort())
    }
    val cache = Cache(2)
    val ddr = DDRSim()

    ddr.io.memPort <> cache.io.memPort
    cache.io.corePort <> io.corePort
  }

  Config.sim.compile {
    val cacheTop = CacheTestTop()
    for (idx <- 0 until 2) {
      cacheTop.cache.cacheMemories(idx).simPublic()
    }
    cacheTop
  }.doSim { dut =>
    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.fallingEdge()
    dut.clockDomain.assertReset()
    dut.clockDomain.waitRisingEdge()
    dut.clockDomain.deassertReset()

    /* 通过接口设置数据 */
    var ramData: Array[Long] = Array.fill(128)(scala.util.Random.nextInt() & 0xFFFFFFFFL)
    for (idx <- 0 until 128) {
      dut.ddr.simMemory.setBigInt(idx, ramData(idx))
    }

    /* 读0x0000_0000的数据 */
    val port = dut.io.corePort
    port.address #= 0
    port.isWrite #= false
    port.writeValue #= 0
    port.valid #= true

    while (port.needStall.toBoolean) {
      dut.clockDomain.waitRisingEdge()
    }
    dut.clockDomain.waitRisingEdge()
    dut.clockDomain.waitRisingEdge()
    dut.clockDomain.waitRisingEdge()
  }
}
