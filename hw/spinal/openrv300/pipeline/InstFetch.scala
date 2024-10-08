package openrv300.pipeline

import openrv300.Config.startAddress
import openrv300.cache._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import payload.{ExecMemPayload, FetchPayload}
import fifo.FetchBufferElement
import openrv300.isa.ExceptionCode
import openrv300.privilege.DoTrapRequest

case class InstFetch() extends Component {
  val io = new Bundle {
    val needReplay = in port Bool()

    val memAnswer = slave(Flow(ExecMemPayload()))
    val dCacheMiss = in port Bool()

    val execNeedStall = in port Bool()
    val csrNeedStall = in port Bool()

    val doTrapPort = slave(DoTrapRequest())

    val iCachePort = master(CacheCorePort())

    val takeJump = in port Bool()
    val jumpAddress = in port UInt(32 bits)

    val fetchBufferPushData = out port FetchPayload()
    val fetchBufferPushValid = out port Bool()
    val fetchBufferHead = in port FetchBufferElement()
    val fetchBufferPop = out port Bool()

    val flush = in port Bool()
    val iCacheIsIdle = out port Bool()
  }

  val programCounter = RegInit(startAddress)

  io.iCachePort.valid := True
  io.iCachePort.address := programCounter
  io.iCachePort.isWrite := False
  io.iCachePort.writeValue := B"32'd0"
  io.iCachePort.writeMask := B"4'd0"
  io.iCachePort.invalidate := io.flush

  val ansPayload = FetchPayload()
  ansPayload.pcAddr := 0
  ansPayload.instruction := 0
  ansPayload.trap.throwTrap := False
  ansPayload.trap.trapPc := 0
  ansPayload.trap.trapCause := 0
  ansPayload.trap.trapValue := 0
  io.fetchBufferPushData := ansPayload
  io.fetchBufferPop := False

  val justReset = Reg(Bool()) init (True)

  val dCacheMissed = RegNext(io.dCacheMiss)
  val fetchValid = Bool()
  fetchValid := False
  io.fetchBufferPushValid := fetchValid

  io.doTrapPort.trapReady := False

  io.iCacheIsIdle := False

  val fsm = new StateMachine {
    val normalWorking = new State with EntryPoint
    val iCacheMiss = new State
    val dCacheMiss = new State
    val execStall = new State
    val csrStall = new State

    normalWorking.whenIsActive {
      io.iCacheIsIdle := True
      when(io.takeJump) {
        programCounter := io.jumpAddress
        fetchValid := False
        /* 清空 fetch buffer，理论上应该是clear，但是这里只存一条指令，所以可以直接pop */
        io.fetchBufferPop := True
      } elsewhen (io.iCachePort.needStall) {
        fetchValid := False
        goto(iCacheMiss)
      } elsewhen (io.csrNeedStall) {
        fetchValid := False
        goto(csrStall)
      } elsewhen (io.execNeedStall) {
        fetchValid := False
        goto(execStall)
      } otherwise {
        /* 遇到源寄存器不满足，需要重放 */
        when(io.needReplay && (!justReset)) {
          when(io.fetchBufferHead.valid) {
            ansPayload := io.fetchBufferHead.payload
            programCounter := io.fetchBufferHead.payload.pcAddr + 4

            fetchValid := True
          }
        } otherwise {
          justReset := False

          /* dCache 没有 Miss，正常取指
          *  任何一个缓存 miss 了都应该暂停流水线
          */
          when(!io.dCacheMiss) {
            ansPayload.instruction := io.iCachePort.readValue
            ansPayload.pcAddr := programCounter
            fetchValid := True
            when(programCounter(1 downto 0) === 0) {
              programCounter := programCounter + 4
            } otherwise {
              /* 非对齐 */
              ansPayload.trap.throwTrap := True
              ansPayload.trap.trapCause := ExceptionCode.InstAddrMisaligned
              ansPayload.trap.trapValue := programCounter.asBits
              ansPayload.trap.trapPc := programCounter
            }
          } otherwise {
            fetchValid := False
            goto(dCacheMiss)
          }
        }
      }
    }

    iCacheMiss.whenIsActive {
      fetchValid := False
      when(io.takeJump) {
        programCounter := io.jumpAddress
        fetchValid := False
      }
      when(!io.iCachePort.needStall) {
        fetchValid := True

        ansPayload.pcAddr := programCounter
        ansPayload.instruction := io.iCachePort.readValue

        programCounter := programCounter + 4
        goto(normalWorking)
      }
    }

    dCacheMiss.whenIsActive {
      fetchValid := False
      when(!io.dCacheMiss && dCacheMissed) {
      io.iCacheIsIdle := True
        /* dCache Miss 刚解决，这个时候应该执行 dCache Miss 时
        * 的下一条指令，并且将本流水级的状态设置到下下条指令
        */
        fetchValid := True

        ansPayload.pcAddr := io.memAnswer.payload.instPc + 4
        ansPayload.instruction := io.iCachePort.readValue
        io.iCachePort.address := io.memAnswer.payload.instPc + 4

        programCounter := io.memAnswer.payload.instPc + 4 + 4
        goto(normalWorking)
      }
    }

    execStall.whenIsActive {
      fetchValid := False
      when(!io.execNeedStall) {
      io.iCacheIsIdle := True
        goto(normalWorking)
      }
    }

    csrStall.whenIsActive {
      fetchValid := False
      io.iCacheIsIdle := True
      /* 清空 fetch buffer，理论上应该是clear，但是这里只存一条指令，所以可以直接pop */
      io.fetchBufferPop := True
      when(io.doTrapPort.trapValid) {
        io.doTrapPort.trapReady := True
        programCounter := io.doTrapPort.trapJumpAddress
        goto(normalWorking)
      } elsewhen (io.csrNeedStall === False) {
        goto(normalWorking)
      }
    }
  }
}
