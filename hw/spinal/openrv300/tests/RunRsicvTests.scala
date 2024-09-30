package openrv300.tests

import openrv300.Config.riscvToolchain
import openrv300.tests.utils.DiffTestUtils._
import openrv300.{Config, OpenRv300SimTop}
import spinal.core._
import spinal.core.sim._

import java.nio.file.{Files, Paths}
import scala.sys.process._

object RunRsicvTests extends App {

  val tests = Seq()

  val cwd = System.getProperty("user.dir")
  val asmFilePath = "hw/spinal/openrv300/tests/bareMetalCPrograms"
  val diffTestEnabled = true


  Config.sim.compile {
    val core = OpenRv300SimTop()
    core.ddr.simMemory.simPublic()
    core.core.fetch.programCounter.simPublic()
    core.core.gprs.registers.simPublic()
    core.core.wb.reqData.simPublic()
    core.core.wb.reqValid.simPublic()
    core
  }.doSim { dut =>
    tests.foreach { tst =>
//      if (diffTestEnabled) {
//        temuReset()
//      }
//
//      /* 寄存器全部设为0 */
//      for (idx <- 0 until 32) {
//        dut.core.gprs.registers.setBigInt(idx, 0)
//      }
//
//      val binFullPath = Paths.get(cwd, Paths.get(asmFilePath, tst.cFile + ".bin").toString).toString
//
//      val makeCmd = s"make CROSS_COMPILE=$riscvToolchain- TEST_FILE=${tst.cFile} -C $asmFilePath"
//      makeCmd.!
//
//      val binaryData = Files.readAllBytes(Paths.get(binFullPath))
//
//      var cnt = 0
//      binaryData.grouped(4).foreach { bytes =>
//        // 小端拼接，将 bytes(0) 放到最低位，bytes(3) 放到最高位
//        val word = (bytes(0) & 0xFF) |
//          ((bytes(1) & 0xFF) << 8) |
//          ((bytes(2) & 0xFF) << 16) |
//          ((bytes(3) & 0xFF) << 24)
//
//        dut.ddr.simMemory.setBigInt(cnt, BigInt(word & 0xFFFFFFFFL))
//
//        if (diffTestEnabled) {
//          val dfAddr = diffTestRamStart + cnt * 4
//          val dfData = BigInt(word & 0xFFFFFFFFL)
//
//          assert(temuWriteSimDDR(dfAddr, dfData))
//        }
//
//        cnt += 1
//      }
//
//      dut.clockDomain.forkStimulus(10)
//      dut.clockDomain.fallingEdge()
//      dut.clockDomain.assertReset()
//      dut.clockDomain.waitRisingEdge()
//      dut.clockDomain.deassertReset()
//
//      var retireCnt = 0
//      var cycles = 0
//
//      var lastInstValid = false
//      var lastInst: BigInt = 0
//      var lastInstPc: BigInt = 0
//
//      while (!(dut.core.wb.reqData.microOp.toInt == 19 && dut.core.wb.reqValid.toBoolean)) {
//        if (lastInstValid && diffTestEnabled && !dut.core.wb.reqData.isNOP.toBoolean && dut.core.wb.reqValid.toBoolean) {
//          lastInstValid = false;
//          val regfile = Array.fill[BigInt](32)(0)
//          for (idx <- 0 until 32) {
//            regfile(idx) = dut.core.gprs.registers.getBigInt(idx)
//          }
//
//          assert(temuRunOneInstAndCompare(lastInst, lastInstPc, dut.core.wb.reqData.instPc.toBigInt, regfile))
//        }
//
//        if (!dut.core.wb.reqData.isNOP.toBoolean && dut.core.wb.reqValid.toBoolean) {
//          retireCnt += 1
//          lastInstValid = true
//          lastInst = dut.core.wb.reqData.instruction.toBigInt
//          lastInstPc = dut.core.wb.reqData.instPc.toBigInt
//        }
//
//        cycles += 1
//        dut.clockDomain.waitRisingEdge()
//      }
//
//      tst.verify(dut)
//
//      val IPC = retireCnt.toDouble / cycles.toDouble
//      println("======= Performance Summary =======")
//      println(f"Instructions retired: $retireCnt\t\tCycles: $cycles")
//      println(f"IPC: $IPC%.4f")
    }
  }
}
