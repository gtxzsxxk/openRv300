package openrv300.tests

import openrv300.Config
import openrv300.cache._
import spinal.core._
import spinal.core.sim._

object CacheTest extends App {
  Config.sim.compile {
    val cache = Cache(2)
    for(idx <- 0 until 2) {
      cache.cacheMemories(idx).simPublic()
    }
    cache
  }.doSim { dut =>
    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.fallingEdge()
    dut.clockDomain.assertReset()
    dut.clockDomain.waitRisingEdge()
    dut.clockDomain.deassertReset()


  }
}
