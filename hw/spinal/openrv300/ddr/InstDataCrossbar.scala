package openrv300.ddr

import openrv300.Config.axiConfig
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._

case class InstDataCrossbar() extends Component {
  /* Round-Robin 仲裁器 */
  val io = new Bundle {
    val iBus = slave(Axi4(axiConfig))
    val dBus = slave(Axi4(axiConfig))
    val coreBus = master(Axi4(axiConfig))
  }

  /* False 代表允许 iBus 通过 */
  val arbiter = Reg(Bool()) init (False)

  val iBusStart = (io.iBus.ar.valid || io.iBus.aw.valid)
  val dBusStart = (io.dBus.ar.valid || io.dBus.aw.valid)

  val isWriting = Reg(Bool())
  val isReading = Reg(Bool())
  val isTransmitting = Reg(Bool())

  val fsm = new StateMachine {
    val idle = new State with EntryPoint
    val transmitting = new State

    idle.onEntry(isTransmitting := False).whenIsActive {
      when(iBusStart && !dBusStart) {
        arbiter := False
      } elsewhen (!iBusStart && dBusStart) {
        arbiter := True
      } otherwise {
        arbiter := ~arbiter
      }

      when(iBusStart || dBusStart) {
        goto(transmitting)
      }
      isWriting := False
      isReading := False
      isTransmitting := False
    }

    transmitting.whenIsActive {
      when(io.coreBus.b.valid) {
        isWriting := True
      } otherwise {
        when(isWriting) {
          /* 写事务结束 */
          goto(idle)
        }
      }

      when(io.coreBus.r.valid) {
        isReading := True
      } otherwise {
        when(isReading) {
          /* 读事务结束 */
          goto(idle)
        }
      }

      isTransmitting := True
    }
  }

  /* 处理 iBus 和 dBus 的 valid/ready */
  io.iBus.aw.ready := io.coreBus.aw.ready && !arbiter
  io.dBus.aw.ready := io.coreBus.aw.ready && arbiter

  io.iBus.ar.ready := io.coreBus.ar.ready && !arbiter
  io.dBus.ar.ready := io.coreBus.ar.ready && arbiter

  io.iBus.w.ready := io.coreBus.w.ready && !arbiter
  io.dBus.w.ready := io.coreBus.w.ready && arbiter

  io.iBus.r.valid := io.coreBus.r.valid && !arbiter
  io.dBus.r.valid := io.coreBus.r.valid && arbiter

  io.iBus.b.valid := io.coreBus.b.valid && !arbiter
  io.dBus.b.valid := io.coreBus.b.valid && arbiter

  def reduceLatch(which: Axi4): Unit = {
    which.b.payload.assignDontCare()
    which.r.payload.assignDontCare()
  }

  reduceLatch(io.iBus)
  reduceLatch(io.dBus)

  when(arbiter) {
    when(isTransmitting) {
      io.coreBus <> io.dBus
    } otherwise {
      io.coreBus.setIdle()
    }
  } otherwise {
    when(isTransmitting) {
      io.coreBus <> io.iBus
    } otherwise {
      io.coreBus.setIdle()
    }
  }
}
