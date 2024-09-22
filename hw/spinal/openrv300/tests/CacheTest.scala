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
    cacheTop.ddr.simMemory.simPublic()
    cacheTop
  }.doSim { dut =>
    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.fallingEdge()
    dut.clockDomain.assertReset()
    dut.clockDomain.waitRisingEdge()
    dut.clockDomain.deassertReset()

    /* 通过接口设置数据 */
    val ramData: Array[Long] = Array.fill(4096)(scala.util.Random.nextInt() & 0xFFFFFFFFL)
    for (idx <- 0 until 4096) {
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
    assert(port.readValue.toLong == ramData(0))
    port.valid #= false
    dut.clockDomain.waitRisingEdge()

    /* 写0x0000_0004的数据 */
    port.address #= 0x0004L
    port.isWrite #= true
    port.writeValue #= ramData(55)
    port.valid #= true
    dut.clockDomain.waitRisingEdge()
    while (port.needStall.toBoolean) {
      dut.clockDomain.waitRisingEdge()
    }
    /* 背靠背执行 */
    //    port.valid #= false
    //    dut.clockDomain.waitRisingEdge()

    /* 读0x0000_0004的数据 */
    port.address #= 0x0004L
    port.isWrite #= false
    port.writeValue #= 0
    port.valid #= true
    dut.clockDomain.waitRisingEdge()
    while (port.needStall.toBoolean) {
      dut.clockDomain.waitRisingEdge()
    }
    assert(port.readValue.toLong == ramData(55))
    port.valid #= false
    dut.clockDomain.waitRisingEdge()

    /* 写0x0000_1004的数据 */
    port.address #= 0x1004L
    port.isWrite #= true
    port.writeValue #= ramData(55)
    port.valid #= true

    while (port.needStall.toBoolean) {
      dut.clockDomain.waitRisingEdge()
    }
    port.valid #= false
    dut.clockDomain.waitRisingEdge()

    /* 读0x0000_0004的数据 */
    port.address #= 0x0004L
    port.isWrite #= false
    port.writeValue #= 0
    port.valid #= true
    dut.clockDomain.waitRisingEdge()
    while (port.needStall.toBoolean) {
      dut.clockDomain.waitRisingEdge()
    }
    assert(port.readValue.toLong == ramData(55))
    port.valid #= false
    dut.clockDomain.waitRisingEdge()

    /* 读0x0000_0004的数据 */
    port.address #= 0x0004L
    port.isWrite #= false
    port.writeValue #= 0
    port.valid #= true
    dut.clockDomain.waitRisingEdge()
    while (port.needStall.toBoolean) {
      dut.clockDomain.waitRisingEdge()
    }
    assert(port.readValue.toLong == ramData(55))
    port.valid #= false
    dut.clockDomain.waitRisingEdge()

    /* 写0x0000_202c的数据，引起页面替换 */
    /* 此时0x0000_1004的数据被写回，检查 */
    /* 1004处在ddr内的数据是否为ramData(55) */
    port.address #= 0x202cL
    port.isWrite #= true
    port.writeValue #= ramData(47)
    port.valid #= true

    while (port.needStall.toBoolean) {
      dut.clockDomain.waitRisingEdge()
    }

    port.valid #= false
    dut.clockDomain.waitRisingEdge()
    assert(dut.ddr.simMemory.getBigInt(0x1004 / 4) == ramData(55))

    /* 写0x0000_3000的数据，测试脏行写回 */
    /* 202c被写回 */
    port.address #= 0x3000L
    port.isWrite #= true
    port.writeValue #= ramData(55)
    port.valid #= true

    while (port.needStall.toBoolean) {
      dut.clockDomain.waitRisingEdge()
    }
    port.valid #= false
    dut.clockDomain.waitRisingEdge()
    assert(dut.ddr.simMemory.getBigInt(0x202c / 4) == ramData(47))
    /* 写0xffff_ffff，引起异常 */
  }
}
