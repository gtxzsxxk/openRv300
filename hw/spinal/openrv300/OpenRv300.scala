package openrv300

import openrv300.regfile.GPRs
import spinal.core._
import spinal.lib.{Flow, master}
import spinal.lib.bus.amba4.axi._
import pipeline._
import pipeline.control.BypassUnit
import cache._
import ddr._
import Config.axiConfig
import openrv300.pipeline.fifo.FetchBuffer
import openrv300.privilege.CSRs

case class OpenRv300() extends Component {
  val io = new Bundle {
    val bus = master(Axi4(axiConfig))
  }

  /* set correct top axi signal names */
  io.bus.setName("")
  io.bus.aw.payload.setName("aw")
  io.bus.ar.payload.setName("ar")
  io.bus.w.payload.setName("w")
  io.bus.r.payload.setName("r")
  io.bus.b.payload.setName("b")

  val fetch = InstFetch()
  val decode = InstDecode()
  val exec = InstExec()
  val mem = MemAccess()
  val wb = WriteBackGPRs()

  val gprs = GPRs()
  val bypassUnit = BypassUnit()
  val fetchBuffer = FetchBuffer(2)

  val iCache = Cache(2)
  val dCache = Cache(2)
  val crossbar = InstDataCrossbar()

  val csrs = CSRs()

  /* 连接流水级 */
  fetchBuffer.io.pushValid := fetch.io.fetchBufferPushValid
  fetchBuffer.io.pushData := fetch.io.fetchBufferPushData
  fetchBuffer.io.pop := decode.io.fetchBufferPop || fetch.io.fetchBufferPop
  fetch.io.fetchBufferHead := fetchBuffer.io.head
  decode.io.fetchBufferHead := fetchBuffer.io.head
  decode.io.answer <> exec.io.request
  exec.io.answer <> mem.io.request
  mem.io.answer <> wb.io.request

  /* 连接译码与写回与GPRs的端口 */
  decode.io.regReadPorts <> gprs.io.decodePorts
  wb.io.regWritePort <> gprs.io.writeBackPort

  /* 数据旁路 */
  decode.io.bypassReadPorts <> bypassUnit.io.execReadPorts
  exec.io.bypassWritePort <> bypassUnit.io.writePort(0)
  mem.io.bypassWritePort <> bypassUnit.io.writePort(1)
  wb.io.bypassWritePort <> bypassUnit.io.writePort(2)

  /* 译码确定好源寄存器，传给执行 */
  decode.io.execRegisters <> exec.io.execRegisters

  /* 译码发现执行需要的源寄存器不满足，需要重放 */
  fetch.io.needReplay := decode.io.waitForSrcReg
  /* dCache 缺失时，停止取指 */
  fetch.io.memAnswer <> mem.io.answer
  fetch.io.dCacheMiss := mem.io.dCacheMiss
  /* exec 执行需要多个周期才能完成的指令时，停止取指 */
  fetch.io.execNeedStall := exec.io.execNeedStall
  decode.io.execNeedStall := exec.io.execNeedStall
  decode.io.execNeedStallInst := exec.io.execNeedStallInst

  /* ==== CSR ==== */
  /* CSR 在访存 */
  csrs.io.port <> mem.io.csrPort
  /* write back 时抛出异常 */
  csrs.io.throwTrapPort <> wb.io.throwTrapPort
  /* 给出 Trap 地址，要求 fetch 跳转 */
  fetch.io.doTrapPort <> csrs.io.doTrapPort
  /* 记录流水线中哪一级发生了异常，停止之后指令 */
  csrs.io.throwTrapNow(0) := decode.io.answer.trap.throwTrap & decode.io.answer.valid
  csrs.io.throwTrapNow(1) := exec.io.answer.trap.throwTrap & exec.io.answer.valid
  csrs.io.throwTrapNow(2) := mem.io.answer.trap.throwTrap & mem.io.answer.valid
  /* decode 处理 ECALL 时，需要知道当前的特权态 */
  decode.io.privilegeLevel := csrs.io.privilegeLevel
  /* 将 csr need stall 信号传给对应的流水线级 */
  fetch.io.csrNeedStall := csrs.io.csrNeedStall(0)
  decode.io.csrNeedStall := csrs.io.csrNeedStall(1)
  exec.io.csrNeedStall := csrs.io.csrNeedStall(2)
  mem.io.csrNeedStall := csrs.io.csrNeedStall(3)
  /* 没有执行的另一个分支的异常需要被清除 */
  csrs.io.clearTrap := exec.io.answer.payload.takeJump

  /* 连接I/D-Cache */
  fetch.io.iCachePort <> iCache.io.corePort
  mem.io.dCachePort <> dCache.io.corePort
  crossbar.io.iBus <> iCache.io.memPort
  crossbar.io.dBus <> dCache.io.memPort
  io.bus <> crossbar.io.coreBus

  /* 源寄存器不满足或者 dCache 缺失时，exec 执行 NOP */
  exec.io.isStalling := decode.io.waitForSrcReg || mem.io.dCacheMiss

  /* 实现跳转 */
  decode.io.takeJump := exec.io.answer.payload.takeJump
  fetch.io.takeJump := exec.io.answer.payload.takeJump
  fetch.io.jumpAddress := exec.io.answer.payload.jumpPc
}
