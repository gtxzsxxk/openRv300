package openrv300.tests

import sys.process._
import java.nio.file.{Files, Paths}
import openrv300.Config.{abi, arch, riscvToolchain}
import openrv300.{Config, OpenRv300}
import spinal.core._
import spinal.core.sim._

object RunSimpleAsmTest extends App {
  case class AssemblyTest(asmFile: String, cycle: Int, verify: (OpenRv300) => Unit)

  val tests = Seq(
    AssemblyTest("StoreAndLoad", 16, (dut) => {
      assert(dut.gprs.registers.getBigInt(0) == BigInt(0))
      assert(dut.gprs.registers.getBigInt(1) == BigInt(4))
      assert(dut.gprs.registers.getBigInt(2) == BigInt(6))
      assert(dut.gprs.registers.getBigInt(3) == BigInt(11))
      assert(dut.gprs.registers.getBigInt(4) == BigInt(13))
    }),
    AssemblyTest("StoreHalfWord", 16, (dut) => {
      assert(dut.gprs.registers.getBigInt(0) == BigInt(0))
      assert(dut.gprs.registers.getBigInt(1) == BigInt(0x12345678L))
      assert(dut.gprs.registers.getBigInt(2) == BigInt(0xfbfbfafaL))
      assert(dut.gprs.registers.getBigInt(3) == BigInt(0x4))
      assert(dut.gprs.registers.getBigInt(4) == BigInt(0x5678fafaL))
    }),
    AssemblyTest("LoadAndLoad", 25, (dut) => {
      assert(dut.gprs.registers.getBigInt(0) == BigInt(0))
      assert(dut.gprs.registers.getBigInt(1) == BigInt(0x04))
      assert(dut.gprs.registers.getBigInt(2) == BigInt(0x08))
      assert(dut.gprs.registers.getBigInt(3) == BigInt(0x99))
      assert(dut.gprs.registers.getBigInt(4) == BigInt(0x99))
      assert(dut.gprs.registers.getBigInt(5) == BigInt(0x9B))
      assert(dut.gprs.registers.getBigInt(6) == BigInt(0x88))
    })
  )

  val cwd = System.getProperty("user.dir")
  val asmFilePath = "hw/spinal/openrv300/tests/assemblyWithNoCRT"
  tests.foreach { tst =>
    val srcFullPath = Paths.get(cwd, Paths.get(asmFilePath, tst.asmFile + ".s").toString).toString
    val objFullPath = Paths.get(cwd, Paths.get(asmFilePath, tst.asmFile + ".o").toString).toString
    val binFullPath = Paths.get(cwd, Paths.get(asmFilePath, tst.asmFile + ".bin").toString).toString

    val compileCmd = s"$riscvToolchain-gcc -c $srcFullPath -o $objFullPath -march=$arch -mabi=$abi"
    compileCmd.!

    val objCopyCmd = s"$riscvToolchain-objcopy -O binary --only-section=.text $objFullPath $binFullPath"
    objCopyCmd.!

    val binaryData = Files.readAllBytes(Paths.get(binFullPath))

    Config.sim.compile {
      val core = OpenRv300()
      core.fetch.instMem.simPublic()
      core.mem.dataMem.simPublic()
      core.fetch.programCounter.simPublic()
      core.gprs.registers.simPublic()
      core
    }.doSim { dut =>
      var cnt = 0
      binaryData.grouped(4).foreach { bytes =>
        // 小端拼接，将 bytes(0) 放到最低位，bytes(3) 放到最高位
        val word = (bytes(0) & 0xFF) |
          ((bytes(1) & 0xFF) << 8) |
          ((bytes(2) & 0xFF) << 16) |
          ((bytes(3) & 0xFF) << 24)

        dut.fetch.instMem.setBigInt(cnt, BigInt(word & 0xFFFFFFFFL))
        cnt += 1
      }

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.fallingEdge()
      dut.clockDomain.assertReset()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.deassertReset()

      println(dut.fetch.instMem.getBigInt(0))

      for (idx <- 0 until tst.cycle) {
        println(dut.fetch.programCounter.toLong)
        dut.clockDomain.waitRisingEdge()
      }

      tst.verify(dut)
    }
  }
}
