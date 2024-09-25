package openrv300.tests.utils

import requests.{get, post}
import ujson.{read, write}

case object DiffTestUtils {
  val diffTestServer = "http://127.0.0.1:5000"
  val diffTestRamStart = 0x80000000L

  def temuReset(): Boolean = {
    val r = get(diffTestServer + "/reset")
    val result: ujson.Value = read(r.text())
    result("valid").bool
  }

  def temuWriteSimDDR(addr: BigInt, data: BigInt): Boolean = {
    val r = get(diffTestServer + f"/ddr/write/$addr%08x/$data%08x")
    val result: ujson.Value = read(r.text())
    result("valid").bool
  }

  def temuRunOneInstAndCompare(inst: BigInt, instPc: BigInt, nextPc: BigInt, registers: Array[BigInt]): Boolean = {
    val r = get(diffTestServer + f"/exec/$inst%08x")
    val result: ujson.Value = read(r.text())
    var returnValue = false
    if (result("valid").bool) {
      var pcOk = true
      if (f"0x$nextPc%08x" != result("pc").str) {
        println("============ Differential Test Failed ============")
        println(f"Program should jump to ${result("pc").str}")
        println(f"But your implementation jumped to 0x$nextPc%08x")
        print("\r\n")
        println(f"Instruction 0x$inst%08x @ 0x$instPc%08x")
        println("==================================================")

        pcOk = false
      }

      var registerOkFlag = true
      var regCnt = 0
      while (regCnt < 32 && registerOkFlag) {
        if (f"0x${registers(regCnt)}%08x" != result("registers")(regCnt).str) {
          registerOkFlag = false
        }
        regCnt += 1
      }

      if (!registerOkFlag) {
        if (pcOk) {
          println("============ Differential Test Failed ============")
        }
        for (line <- 0 until 4) {
          for (col <- 0 until 8) {
            val regIdx = line * 8 + col
            print(f"x$regIdx%02d: ${registers(regIdx)}%08x\t")
          }
          print("\r\n")
        }
        print("\r\n")

        regCnt = 0
        while (regCnt < 32) {
          if (f"0x${registers(regCnt)}%08x" != result("registers")(regCnt).str) {
            println(f"x$regCnt%02d should be ${result("registers")(regCnt).str} but it is 0x${registers(regCnt)}%08x")
          }
          regCnt += 1
        }

        println("\r\n")
        println(f"Instruction 0x$inst%08x @ 0x$instPc%08x")
        println("==================================================")
      }

      returnValue = pcOk && registerOkFlag
    }
    returnValue
  }
}
