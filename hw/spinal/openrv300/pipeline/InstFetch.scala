package openrv300.pipeline

import spinal.core._
import spinal.lib._
import payload.FetchPayload

case class InstFetch() extends Component {
  val io = new Bundle {
    val answer = master(Flow(FetchPayload()))
  }

  val instMem = Mem(Bits(32 bits), wordCount = 256)
  val programCounter = RegInit(U"h8000_0000")

  val payload = Reg(FetchPayload())
  payload.pcAddr := programCounter
  payload.instruction := instMem(programCounter(9 downto 2)).asUInt
  io.answer.push(payload)

  programCounter := programCounter + 4
}
