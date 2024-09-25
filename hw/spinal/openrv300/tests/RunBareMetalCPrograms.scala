package openrv300.tests

import sys.process._
import java.nio.file.{Files, Paths}
import openrv300.Config.{abi, arch, riscvToolchain}
import openrv300.isa.MicroOp
import openrv300.{Config, OpenRv300}
import spinal.core._
import spinal.core.sim._

object RunBareMetalCPrograms extends App {
  case class BareMetalCProgram(cFile: String, verify: (OpenRv300) => Unit)

  val tests = Seq(
    BareMetalCProgram("test1", dut => {
      assert(dut.gprs.registers.getBigInt(10) == BigInt(9))
    })
  )

  val cwd = System.getProperty("user.dir")
  val asmFilePath = "hw/spinal/openrv300/tests/bareMetalCPrograms"

  tests.foreach { tst =>
    val binFullPath = Paths.get(cwd, Paths.get(asmFilePath, tst.cFile + ".bin").toString).toString

    val makeCmd = s"make CROSS_COMPILE=$riscvToolchain- TEST_FILE=${tst.cFile} -C $asmFilePath"
    makeCmd.!

    val binaryData = Files.readAllBytes(Paths.get(binFullPath))

    Config.sim.compile {
      val core = OpenRv300()
      core.ddr.simMemory.simPublic()
      core.fetch.programCounter.simPublic()
      core.gprs.registers.simPublic()
      core.wb.reqData.simPublic()
      core
    }.doSim { dut =>
      var cnt = 0
      binaryData.grouped(4).foreach { bytes =>
        // 小端拼接，将 bytes(0) 放到最低位，bytes(3) 放到最高位
        val word = (bytes(0) & 0xFF) |
          ((bytes(1) & 0xFF) << 8) |
          ((bytes(2) & 0xFF) << 16) |
          ((bytes(3) & 0xFF) << 24)

        dut.ddr.simMemory.setBigInt(cnt, BigInt(word & 0xFFFFFFFFL))
        cnt += 1
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.fallingEdge()
      dut.clockDomain.assertReset()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.deassertReset()

      var retireCnt = 0
      var cycles = 0
      while(dut.wb.reqData.microOp.toInt != 19) {
        if(!dut.wb.reqData.isNOP.toBoolean) {
          retireCnt += 1
        }
        cycles += 1
        dut.clockDomain.waitRisingEdge()
      }

      tst.verify(dut)

      val IPC = retireCnt.toDouble / cycles.toDouble
      println("======= Performance Summary =======")
      println(f"Instructions retired: $retireCnt\t\tCycles: $cycles")
      println(f"IPC: $IPC%.4f")
    }
  }
}
