package openrv300.ddr

import openrv300.Config.axiConfig
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm._

case class DDRSim() extends Component {
  val io = new Bundle {
    val memPort = slave(Axi4(axiConfig))
  }

  val aw = io.memPort.aw
  val w = io.memPort.w
  val b = io.memPort.b
  val ar = io.memPort.ar
  val r = io.memPort.r

  aw.setBlocked()
  w.setBlocked()
  b.setIdle()
  ar.setBlocked()
  r.setIdle()

  val simMemory = Mem(Bits(32 bits), wordCount = 0x02000000)

  val fsm = new StateMachine {
    val axiId = Reg(UInt(4 bits))
    val addr = Reg(UInt(32 bits))
    val addrByWord = Cat(addr(31 downto 2), U"2'd0").asUInt
    val len = Reg(UInt(8 bits))
    val size = Reg(UInt(3 bits))
    val burst = Reg(Bits(2 bits))

    val counter = Reg(UInt(8 bits))

    val init = new State with EntryPoint
    val doWrite = new State
    val writeResponse = new State
    val doRead = new State

    init.whenIsActive {
      r.setIdle()

      when(aw.valid) {
        val compatible = aw.payload.burst === Axi4.burst.INCR && aw.payload.size === Axi4.size.BYTE_4.asUInt

        axiId := aw.payload.id
        addr := aw.payload.addr
        len := aw.payload.len
        size := aw.payload.size
        burst := aw.payload.burst
        aw.ready := compatible

        when(compatible) {
          goto(doWrite)
        }
      } elsewhen (ar.valid) {
        val compatible = ar.payload.burst === Axi4.burst.INCR && ar.payload.size === Axi4.size.BYTE_4.asUInt
        axiId := ar.payload.id
        addr := ar.payload.addr
        len := ar.payload.len
        size := ar.payload.size
        burst := ar.payload.burst
        ar.ready := compatible

        when(compatible) {
          goto(doRead)
        }
      }
    }

    doWrite.onEntry(counter := 0).whenIsActive {
      w.ready := True
      when(w.valid) {
        simMemory.write((addrByWord + counter).resized, w.payload.data)
        counter := counter + 4
        when(w.last) {
          goto(writeResponse)
        }
      }
    }

    writeResponse.whenIsActive {
      b.payload.resp := Axi4.resp.OKAY
      b.valid := True
      b.payload.id := axiId
      when(b.ready) {
        goto(init)
      }
    }

    doRead.onEntry(counter := 0).whenIsActive {
      when(r.ready) {
        val last = counter === (len + 1 - 4)
        r.payload.resp := Axi4.resp.OKAY
        r.payload.data := simMemory((addrByWord + counter).resized)
        r.valid := True
        r.last := counter === (len + 1 - 4)
        counter := counter + 4

        when(last) {
          goto(init)
        }
      }
    }
  }
}
