package openrv300.pipeline.muldiv

import openrv300.pipeline.payload.{DecodePayload, ExecMemPayload, RegisterSourceBundle}
import openrv300.pipeline.muldiv.Multiplier
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class MulDivExec(pipelineStages: Int) extends Component {
  val io = new Bundle {
    /* 此处的 valid 为执行单元给出的 */
    /* 为组合逻辑 */
    val request = slave(Flow(DecodePayload()))
    val execRegisters = in port Vec.fill(2)(RegisterSourceBundle())
    val answer = master(Flow(ExecMemPayload()))
  }

  val reqData = io.request.payload
  val ansPayload = ExecMemPayload()
  io.answer.payload := ansPayload

  ansPayload.microOp := reqData.microOp
  ansPayload.instPc := reqData.instPc
  ansPayload.instruction := reqData.instruction
  ansPayload.isNOP := False
  ansPayload.function0 := reqData.function0
  ansPayload.function1 := reqData.function1
  ansPayload.regDest := reqData.regDest
  ansPayload.registerSources := io.execRegisters
  ansPayload.imm := reqData.imm
  ansPayload.sextImm := reqData.sextImm
  ansPayload.writeRegDest := True
  ansPayload.regDestValue := 0
  ansPayload.takeJump := False
  ansPayload.jumpPc := 0
  ansPayload.memoryAddress := 0

  /* 由于是五级流水线，按序单发射 */
  /* 因此这里需要停顿流水线，并且要求IF,ID重放 */
  /* 只能实现为多周期的乘法执行单元 */
  val multiplier = Multiplier()

  def signedToUnsigned(value: Bits): UInt = {
    val ret = UInt(32 bits)
    when(value(31)) {
      ret := (~value).asUInt + U"32'd1"
    } otherwise {
      ret := value
    }
    ret
  }

  def unsignedToSigned(value: UInt): Bits = {
    (~value + U"1").asBits
  }

  def getOutputSign(value1: Bool, value2: Bool): Bool = {
    value1 ^ value2
  }

  val fsm = new StateMachine {
    val waitCounter = Reg(UInt(3 bits))
    val outputSign = Reg(Bool())
    val fsmRequest = Reg(DecodePayload())
    val idle = new State with EntryPoint
    val working = new State

    idle.whenIsActive {
      when(io.request.valid) {
        fsmRequest := io.request
        /* 做乘法时全部转换为无符号数 */
        switch(reqData.function0) {
          is(B"000") {
            /* MUL */
            multiplier.io.A := io.execRegisters(0).value
            multiplier.io.B := io.execRegisters(1).value
            outputSign := False
          }
          is(B"001") {
            /* MULH */
            multiplier.io.A := signedToUnsigned(io.execRegisters(0).value)
            multiplier.io.B := signedToUnsigned(io.execRegisters(1).value)
            outputSign := getOutputSign(io.execRegisters(0).value(31), io.execRegisters(1).value(31))
          }
          is(B"010") {
            /* MULHSU */
            multiplier.io.A := signedToUnsigned(io.execRegisters(0).value)
            multiplier.io.B := io.execRegisters(1).value
            outputSign := io.execRegisters(0).value(31)
          }
          is(B"011") {
            /* MULHU */
            multiplier.io.A := io.execRegisters(0).value
            multiplier.io.B := io.execRegisters(1).value
            outputSign := False
          }
          default {
            multiplier.io.A := io.execRegisters(0)
            multiplier.io.B := io.execRegisters(1)
          }
        }
        waitCounter := 3 - 1
        goto(working)
      }
      io.answer.valid := False
    }

    working.whenIsActive {
      io.answer.valid := False
      when(waitCounter === 0) {
        io.answer.valid := True
        switch(fsmRequest.function0) {
          is(B"000") {
            /* MUL */
            ansPayload.regDestValue := multiplier.io.P(31 downto 0)
          }
          is(B"001", B"010", B"011") {
            /* MULH, MULHSU, MULHU */
            when(outputSign) {
              ansPayload.regDestValue := unsignedToSigned(multiplier.io.P)(63 downto 32)
            } otherwise {
              ansPayload.regDestValue := multiplier.io.P(63 downto 32)
            }
          }
        }
        goto(working)
      }
      waitCounter := waitCounter - 1
    }
  }
}
