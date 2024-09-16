package openrv300

import openrv300.regfile.GPRs
import spinal.core._
import spinal.lib.{Flow, master}
import spinal.lib.bus.amba4.axi._
import pipeline._
import pipeline.control.BypassUnit

case class OpenRv300() extends Component {
  val io = new Bundle {
    val bus = master(Axi4(Axi4Config(
      addressWidth = 32,
      dataWidth = 32,
      idWidth = 4,
      useRegion = false,
      useLock = false,
      useCache = false,
      useQos = false,
      useProt = false,
    )))
  }

  /* set correct top axi signal names */
  io.bus.setName("")
  io.bus.aw.payload.setName("aw")
  io.bus.ar.payload.setName("ar")
  io.bus.w.payload.setName("w")
  io.bus.r.payload.setName("r")
  io.bus.b.payload.setName("b")

  /* dummy device */
  io.bus.setIdle()

  val fetch = InstFetch()
  val decode = InstDecode()
  val exec = InstExec()
  val mem = MemAccess()
  val wb = WriteBackGPRs()

  val gprs = GPRs()
  val bypassUnit = BypassUnit()


  fetch.io.answer <> decode.io.request
  decode.io.answer <> exec.io.request
  exec.io.answer <> mem.io.request
  mem.io.answer <> wb.io.request

  decode.io.regReadPorts <> gprs.io.decodePorts
  wb.io.regWritePort <> gprs.io.writeBackPort

  decode.io.bypassReadPorts <> bypassUnit.io.execReadPorts
  exec.io.bypassWritePort <> bypassUnit.io.writePort(0)
  mem.io.bypassWritePort <> bypassUnit.io.writePort(1)
  wb.io.bypassWritePort <> bypassUnit.io.writePort(2)

  decode.io.execRegisters <> exec.io.execRegisters

  fetch.io.noReplay := decode.io.answer.valid
}
