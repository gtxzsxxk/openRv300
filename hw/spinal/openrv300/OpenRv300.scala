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

  val iCache = Cache(2)
  val dCache = Cache(2)
  val crossbar = InstDataCrossbar()

  /* 连接流水级 */
  fetch.io.answer <> decode.io.request
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
