package openrv300.tests

import spinal.core._
import spinal.core.sim._
import openrv300.{Config, OpenRv300}

object AddImmTest extends App {
  Config.sim.compile {
    val core = OpenRv300()
    core.ddr.simMemory.simPublic()
    core.fetch.programCounter.simPublic()
    core.gprs.registers.simPublic()
    core
  }.doSim { dut =>
    /*
    * Test ADDI
    * addi x1 , x0,   1000  /* x1  = 1000 0x3E8 */
    * addi x2 , x1,   2000  /* x2  = 3000 0xBB8 */
    * addi x3 , x2,  -1000  /* x3  = 2000 0x7D0 */
    * addi x4 , x3,  -2000  /* x4  = 0    0x000 */
    * addi x5 , x4,   1000  /* x5  = 1000 0x3E8 */
    * addi x6 , x4,   555   /* x6  = 555  0x22B */
    * addi x7 , x4,   666   /* x7  = 666  0x29A */
    * addi x8 , x4,   777   /* x8  = 777  0x309 */
    */
    dut.ddr.simMemory.setBigInt(0, BigInt("3e800093", 16))
    dut.ddr.simMemory.setBigInt(1, BigInt("7d008113", 16))
    dut.ddr.simMemory.setBigInt(2, BigInt("c1810193", 16))
    dut.ddr.simMemory.setBigInt(3, BigInt("83018213", 16))
    dut.ddr.simMemory.setBigInt(4, BigInt("3e820293", 16))
    dut.ddr.simMemory.setBigInt(5, BigInt("22b20313", 16))
    dut.ddr.simMemory.setBigInt(6, BigInt("29a20393", 16))
    dut.ddr.simMemory.setBigInt(7, BigInt("30920413", 16))

    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.fallingEdge()
    dut.clockDomain.assertReset()
    dut.clockDomain.waitRisingEdge()
    dut.clockDomain.deassertReset()

    assert(dut.ddr.simMemory.getBigInt(0) == 0x3e800093L)

    for (idx <- 0 until 32) {
      dut.clockDomain.waitRisingEdge()
    }

    assert(dut.gprs.registers.getBigInt(0) == BigInt(0))
    assert(dut.gprs.registers.getBigInt(1) == BigInt(1000))
    assert(dut.gprs.registers.getBigInt(2) == BigInt(3000))
    assert(dut.gprs.registers.getBigInt(3) == BigInt(2000))
    assert(dut.gprs.registers.getBigInt(4) == BigInt(0))
    assert(dut.gprs.registers.getBigInt(5) == BigInt(1000))
    assert(dut.gprs.registers.getBigInt(6) == BigInt(555))
    assert(dut.gprs.registers.getBigInt(7) == BigInt(666))
    assert(dut.gprs.registers.getBigInt(8) == BigInt(777))
  }
}
