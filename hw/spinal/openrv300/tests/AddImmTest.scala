package openrv300.tests

import spinal.core._
import spinal.core.sim._
import openrv300.{Config, OpenRv300}

object AddImmTest extends App {
  Config.sim.compile {
    val core = OpenRv300()
    core.fetch.instMem.simPublic()
    core.mem.dataMem.simPublic()
    core.fetch.programCounter.simPublic()
    core
  }.doSim { dut =>
    /*
    * Test ADDI
    * addi x1 , x0,   1000  /* x1  = 1000 0x3E8 */
    * addi x2 , x1,   2000  /* x2  = 3000 0xBB8 */
    * addi x3 , x2,  -1000  /* x3  = 2000 0x7D0 */
    * addi x4 , x3,  -2000  /* x4  = 0    0x000 */
    * addi x5 , x4,   1000  /* x5  = 1000 0x3E8 */
    */
    dut.fetch.instMem.setBigInt(0, BigInt("3e800093", 16))
    dut.fetch.instMem.setBigInt(1, BigInt("7d008113", 16))
    dut.fetch.instMem.setBigInt(2, BigInt("c1810193", 16))
    dut.fetch.instMem.setBigInt(3, BigInt("83018213", 16))
    dut.fetch.instMem.setBigInt(4, BigInt("3e820293", 16))

    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.fallingEdge()
    dut.clockDomain.assertReset()
    dut.clockDomain.waitRisingEdge()
    dut.clockDomain.deassertReset()

    println(dut.fetch.instMem.getBigInt(0))

    for (idx <- 0 until 15) {
      println(dut.fetch.programCounter.toLong)
      dut.clockDomain.waitRisingEdge()
    }
  }
}
