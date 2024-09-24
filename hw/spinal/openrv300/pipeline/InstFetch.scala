package openrv300.pipeline

import openrv300.Config.startAddress
import openrv300.cache._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import payload.{ExecMemPayload, FetchPayload}

case class InstFetch() extends Component {
  val io = new Bundle {
    val needReplay = in port Bool()
    val memAnswer = slave(Flow(ExecMemPayload()))
    val dCacheMiss = in port Bool()
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
  io.answer.payload := payload

  val justReset = Reg(Bool()) init (True)

  val dCacheMissed = RegNext(io.dCacheMiss)
  val fetchValid = Reg(Bool())
  fetchValid := True
  io.answer.valid := fetchValid

  val fsm = new StateMachine {
    val normalWorking = new State with EntryPoint
    val iCacheMiss = new State
    val dCacheMiss = new State

    normalWorking.whenIsActive {
      when(io.iCachePort.needStall) {
        fetchValid := False
        goto(iCacheMiss)
      } otherwise {
        /* 遇到源寄存器不满足，需要重放 */
        when(io.needReplay && (!justReset)) {
          payload.pcAddr := payload.pcAddr
          payload.instruction := payload.instruction

          programCounter := payload.pcAddr + 4
        } otherwise {
          justReset := False

          /* dCache 没有 Miss，正常取指
          *  任何一个缓存 miss 了都应该暂停流水线
          */
          when(!io.dCacheMiss){

          } otherwise {
            fetchValid := False
          }
        }
      }
    }

    iCacheMiss.whenIsActive {
      fetchValid := False
      when(!io.iCachePort.needStall) {
        io.answer.valid := True

      }
    }
  }

  when(io.needReplay && (!justReset)) {
    payload.pcAddr := payload.pcAddr
    payload.instruction := payload.instruction

    programCounter := payload.pcAddr + 4
  } otherwise {
    justReset := False

    /* 正确的信号，代表dcache正在重填 */
    when(!io.dCacheMiss) {
      payload.instruction := io.iCachePort.readValue
      io.iCachePort.valid := True
      when(!dCacheMissed) {
        payload.pcAddr := programCounter

        when(io.iCachePort.needStall) {
          fetchValid := False
        } otherwise {
          programCounter := programCounter + 4
        }
      } otherwise {
        /* dCache Miss 刚解决，这个时候应该执行 dCache Miss 时
        * 的下一条指令，并且将本流水级的状态设置到下下条指令
        */
        payload.pcAddr := io.memAnswer.payload.instPc + 4
        io.iCachePort.address := io.memAnswer.payload.instPc + 4

        when(io.iCachePort.needStall) {
          fetchValid := False
        } otherwise {
          programCounter := io.memAnswer.payload.instPc + 4 + 4
        }
      }
    } otherwise {
      fetchValid := False
    }
  }
}
