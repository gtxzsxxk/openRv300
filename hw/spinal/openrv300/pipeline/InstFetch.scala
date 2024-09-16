package openrv300.pipeline

import spinal.core._
import spinal.lib._
import payload.FetchPayload

case class InstFetch() extends Component {
  val io = new Bundle {
    val noReplay = in port Bool()
    val answer = master(Flow(FetchPayload()))
  }

  val instMem = Mem(Bits(32 bits), wordCount = 256)
  val programCounter = RegInit(U"32'h0000_0000")

  val payload = Reg(FetchPayload())
  io.answer.push(payload)

  val justReset = Reg(Bool()) init (True)

  when(!io.noReplay && (!justReset)) {
    payload.pcAddr := payload.pcAddr
    payload.instruction := payload.instruction

    programCounter := payload.pcAddr + 4
  } otherwise {
    justReset := False
    payload.pcAddr := programCounter
    payload.instruction := instMem(programCounter(9 downto 2))

    programCounter := programCounter + 4
  }
}
