package openrv300.pipeline

import openrv300.Config.startAddress
import openrv300.cache._
import spinal.core._
import spinal.lib._
import payload.FetchPayload

case class InstFetch() extends Component {
  val io = new Bundle {
    val needReplay = in port Bool()
    val answer = master(Flow(FetchPayload()))
    val iCachePort = master(CacheCorePort())
  }

  val programCounter = RegInit(startAddress)

  io.iCachePort.valid := False
  io.iCachePort.address := programCounter
  io.iCachePort.isWrite := False
  io.iCachePort.writeValue := B"32'd0"
  io.iCachePort.writeMask := B"4'd0"

  val payload = Reg(FetchPayload())
  io.answer.push(payload)

  val justReset = Reg(Bool()) init (True)

  when(io.needReplay && (!justReset)) {
    payload.pcAddr := payload.pcAddr
    payload.instruction := payload.instruction

    programCounter := payload.pcAddr + 4
  } otherwise {
    justReset := False
    payload.pcAddr := programCounter
    payload.instruction := io.iCachePort.readValue

    when(io.iCachePort.needStall) {
      io.answer.setIdle()
    } otherwise {
      programCounter := programCounter + 4
    }
  }
}
