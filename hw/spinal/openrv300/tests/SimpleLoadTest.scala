package openrv300.tests

import openrv300.{Config, OpenRv300}
import spinal.core._
import spinal.core.sim._

object SimpleLoadTest extends App {
  Config.sim.compile {
    val core = OpenRv300()
    core.ddr.simMemory.simPublic()
    core.fetch.programCounter.simPublic()
    core.gprs.registers.simPublic()
    core
  }.doSim { dut =>
    /*
    * addi x1 , x0,   4       /* x1  = 0x4 */
    * addi x2 , x1,   0x112   /* x2  = 0x116 */
    * sw   x2 , 0(x1)         /* store */
    * lb   x3 , 0(x1)         /* x3  = 0x16 */
    * add  x3 , x3,   x1      /* x3  = 0x1A */
    * addi x4 , x3,   0x1     /* x4  = 0x1B */
    */
    dut.ddr.simMemory.setBigInt(0, BigInt("00400093", 16))
    dut.ddr.simMemory.setBigInt(1, BigInt("11208113", 16))
    dut.ddr.simMemory.setBigInt(2, BigInt("0020a023", 16))
    dut.ddr.simMemory.setBigInt(3, BigInt("00008183", 16))
    dut.ddr.simMemory.setBigInt(4, BigInt("001181b3", 16))
    dut.ddr.simMemory.setBigInt(5, BigInt("00118213", 16))

    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.fallingEdge()
    dut.clockDomain.assertReset()
    dut.clockDomain.waitRisingEdge()
    dut.clockDomain.deassertReset()

    for (idx <- 0 until 60) {
      println(dut.fetch.programCounter.toLong)
      dut.clockDomain.waitRisingEdge()
    }

    assert(dut.gprs.registers.getBigInt(0) == BigInt(0))
    assert(dut.gprs.registers.getBigInt(1) == BigInt(4))
    assert(dut.gprs.registers.getBigInt(2) == BigInt(278))
    assert(dut.gprs.registers.getBigInt(3) == BigInt(26))
    assert(dut.gprs.registers.getBigInt(4) == BigInt(27))
  }
}
