package openrv300.pipeline.fifo

import openrv300.pipeline.payload.FetchPayload
import spinal.core._
import spinal.lib._

case class FetchBuffer(depth: Int) extends Component {
  val io = new Bundle {
    val pushData = in port FetchPayload()
    val pushValid = in port Bool()
    val pop = in port Bool()
    val head = out port FetchBufferElement()
  }

  val tailPointer = Reg(UInt(log2Up(depth) bits)) init (0)

  val slots = Reg(Vec.fill(depth)(FetchBufferElement()))

  for(idx <- 0 until depth) {
    slots(idx).valid init (False)
  }

  //  when(io.clear) {
  //    tailPointer := 0
  //    for (idx <- 0 until depth) {
  //      slots(idx).valid := False
  //    }
  //  } otherwise {
  when(io.pop && !io.pushValid) {
    for (idx <- 0 until depth - 1) {
      slots(idx) := slots(idx + 1)
    }
    when(tailPointer > 0) {
      tailPointer := tailPointer - 1
    }
  } elsewhen (!io.pop && io.pushValid) {
    slots(tailPointer).payload := io.pushData
    slots(tailPointer).valid := True
  } elsewhen (io.pop && io.pushValid) {
    for (idx <- 0 until depth - 1) {
      when(U(idx).resized === tailPointer) {
        slots(idx).payload := io.pushData
        slots(idx).valid := True
      } otherwise {
        slots(idx) := slots(idx + 1)
      }
    }
  }
  //  }

  io.head := slots(0)
}
