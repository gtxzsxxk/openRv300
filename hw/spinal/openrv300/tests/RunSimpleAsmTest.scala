package openrv300.tests

import sys.process._
import java.nio.file.{Files, Paths}
import openrv300.Config.{abi, arch, riscvToolchain}
import openrv300.{Config, OpenRv300SimTop}
import spinal.core._
import spinal.core.sim._

object RunSimpleAsmTest extends App {
  case class AssemblyTest(asmFile: String, cycle: Int, verify: (OpenRv300SimTop) => Unit)

  val tests = Seq(
    AssemblyTest("StoreAndLoad", 80, (dut) => {
      assert(dut.core.gprs.registers.getBigInt(1) == BigInt(0x80001000L))
      assert(dut.core.gprs.registers.getBigInt(2) == BigInt(6))
      assert(dut.core.gprs.registers.getBigInt(3) == BigInt(6))
      assert(dut.core.gprs.registers.getBigInt(4) == BigInt(8))
    }),
    AssemblyTest("StoreHalfWord", 80, (dut) => {
      assert(dut.core.gprs.registers.getBigInt(1) == BigInt(0x12345678L))
      assert(dut.core.gprs.registers.getBigInt(2) == BigInt(0xfbfbfafaL))
      assert(dut.core.gprs.registers.getBigInt(3) == BigInt(0x80001000L))
      assert(dut.core.gprs.registers.getBigInt(4) == BigInt(0x5678fafaL))
    }),
    AssemblyTest("LoadAndLoad", 150, (dut) => {
      assert(dut.core.gprs.registers.getBigInt(1) == BigInt(0x80000f04L))
      assert(dut.core.gprs.registers.getBigInt(2) == BigInt(0x80001f04L))
      assert(dut.core.gprs.registers.getBigInt(3) == BigInt(0x80000f14L))
      assert(dut.core.gprs.registers.getBigInt(4) == BigInt(0x80001f14L))
      assert(dut.core.gprs.registers.getBigInt(5) == BigInt(0x9D))
      assert(dut.core.gprs.registers.getBigInt(6) == BigInt(0x9C))
    }),
    AssemblyTest("OneSumTo100", 640, (dut) => {
      assert(dut.core.gprs.registers.getBigInt(10) == BigInt(5050))
      assert(dut.core.gprs.registers.getBigInt(11) == BigInt(101))
      assert(dut.core.gprs.registers.getBigInt(5) == BigInt(101))
    })
  )

  val cwd = System.getProperty("user.dir")
  val asmFilePath = "hw/spinal/openrv300/tests/assemblyWithNoCRT"

  Config.sim.compile {
    val core = OpenRv300SimTop()
    core.ddr.simMemory.simPublic()
    core.core.fetch.programCounter.simPublic()
    core.core.gprs.registers.simPublic()
    core.core.wb.reqData.simPublic()
    core
  }.doSim { dut =>
    tests.foreach { tst =>
      val srcFullPath = Paths.get(cwd, Paths.get(asmFilePath, tst.asmFile + ".s").toString).toString
      val objFullPath = Paths.get(cwd, Paths.get(asmFilePath, tst.asmFile + ".o").toString).toString
      val binFullPath = Paths.get(cwd, Paths.get(asmFilePath, tst.asmFile + ".bin").toString).toString

      val compileCmd = s"$riscvToolchain-gcc -c $srcFullPath -o $objFullPath -march=$arch -mabi=$abi"
      compileCmd.!

      val objCopyCmd = s"$riscvToolchain-objcopy -O binary --only-section=.text $objFullPath $binFullPath"
      objCopyCmd.!

      val binaryData = Files.readAllBytes(Paths.get(binFullPath))

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
      for (idx <- 0 until tst.cycle) {
        if (!dut.core.wb.reqData.isNOP.toBoolean) {
          retireCnt += 1
        }
        dut.clockDomain.waitRisingEdge()
      }

      tst.verify(dut)

      val IPC = retireCnt.toDouble / tst.cycle.toDouble
      println("======= Performance Summary =======")
      println(f"Instructions retired: $retireCnt\t\tCycles: ${tst.cycle}")
      println(f"IPC: $IPC%.4f")
    }
  }
}
