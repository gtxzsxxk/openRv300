package openrv300.utils

import openrv300.Config.axiConfig
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm._

class DDRSim extends Component {
  val io = new Bundle {
    val memPort = slave(Axi4(axiConfig))
  }

  val aw = io.memPort.aw
  val w = io.memPort.w
  val b = io.memPort.b
  val ar = io.memPort.ar
  val r = io.memPort.r

  val simMemory = Mem(Bits(32 bits), wordCount = 0x02000000)

  val fsm = new StateMachine {
    val addr = Reg(UInt(32 bits))
    val len = Reg(Bits(8 bits))
    val size = Reg(Bits(3 bits))
    val burst = Reg(Bits(2 bits))
    val ready = Reg(Bool())

    val init = new State with EntryPoint
    val goWrite = new State
    val goRead = new State

    init.whenIsActive {
      when(aw.valid) {
        addr := aw.addr

      }
    }
  }
}
