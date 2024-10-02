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
  val divider = Divider()

  def signedToUnsigned(value: Bits): UInt = {
    val ret = UInt(32 bits)
    when(value(31)) {
      ret := (~value).asUInt + U"32'd1"
    } otherwise {
      ret := value.asUInt
    }
    ret
  }

  def unsignedToSigned(value: UInt): Bits = {
    (~value + U"1").asBits
  }

  def getOutputSign(value1: Bool, value2: Bool): Bool = {
    value1 ^ value2
  }

  io.answer.valid := False

  multiplier.io.A := U"0".resized
  multiplier.io.B := U"0".resized
  divider.io.s_axis_dividend_tdata := 0
  divider.io.s_axis_divisor_tdata := 0
  divider.io.s_axis_dividend_tvalid := False
  divider.io.s_axis_divisor_tvalid := False

  val fsm = new StateMachine {
    val waitCounter = Reg(UInt(3 bits))
    val isDivRem = Reg(Bool())
    val isRem = Reg(Bool())
    val outputSign = Reg(Bool())
    val fsmRequest = Reg(DecodePayload())
    val idle = new State with EntryPoint
    val working = new State

    idle.whenIsActive {
      when(io.request.valid) {
        fsmRequest := reqData
        isRem := False
        /* 做乘法时全部转换为无符号数 */
        switch(reqData.function0) {
          is(B"000") {
            /* MUL */
            multiplier.io.A := io.execRegisters(0).value.asUInt
            multiplier.io.B := io.execRegisters(1).value.asUInt
            outputSign := False
            isDivRem := False
          }
          is(B"001") {
            /* MULH */
            multiplier.io.A := signedToUnsigned(io.execRegisters(0).value)
            multiplier.io.B := signedToUnsigned(io.execRegisters(1).value)
            outputSign := getOutputSign(io.execRegisters(0).value(31), io.execRegisters(1).value(31))
            isDivRem := False
          }
          is(B"010") {
            /* MULHSU */
            multiplier.io.A := signedToUnsigned(io.execRegisters(0).value)
            multiplier.io.B := io.execRegisters(1).value.asUInt
            outputSign := io.execRegisters(0).value(31)
            isDivRem := False
          }
          is(B"011") {
            /* MULHU */
            multiplier.io.A := io.execRegisters(0).value.asUInt
            multiplier.io.B := io.execRegisters(1).value.asUInt
            outputSign := False
            isDivRem := False
          }
          is(B"100", B"110") {
            /* DIV, REM */
            divider.io.s_axis_dividend_tdata := signedToUnsigned(io.execRegisters(0).value)
            divider.io.s_axis_divisor_tdata := signedToUnsigned(io.execRegisters(1).value)
            divider.io.s_axis_dividend_tvalid := True
            divider.io.s_axis_divisor_tvalid := True
            outputSign := getOutputSign(io.execRegisters(0).value(31), io.execRegisters(1).value(31))
            isDivRem := True
            when(reqData.function0 === B"110") {
              outputSign := io.execRegisters(0).value(31)
              isRem := True
            }
          }
          is(B"101", B"111") {
            /* DIVU, REMU */
            divider.io.s_axis_dividend_tdata := io.execRegisters(0).value.asUInt
            divider.io.s_axis_divisor_tdata := io.execRegisters(1).value.asUInt
            divider.io.s_axis_dividend_tvalid := True
            divider.io.s_axis_divisor_tvalid := True
            outputSign := False
            isDivRem := True
            when(reqData.function0 === B"111") {
              isRem := True
            }
          }
        }
        waitCounter := pipelineStages - 1
        goto(working)
      }
      io.answer.valid := False
    }

    working.whenIsActive {
      io.answer.valid := False

      ansPayload.microOp := fsmRequest.microOp
      ansPayload.instPc := fsmRequest.instPc
      ansPayload.instruction := fsmRequest.instruction
      ansPayload.function0 := fsmRequest.function0
      ansPayload.function1 := fsmRequest.function1
      ansPayload.regDest := fsmRequest.regDest
      ansPayload.imm := fsmRequest.imm
      ansPayload.sextImm := fsmRequest.sextImm

      when(!isDivRem) {
        when(waitCounter === 0) {
          switch(fsmRequest.function0) {
            is(B"000") {
              /* MUL */
              ansPayload.regDestValue := multiplier.io.P(31 downto 0).asBits
            }
            is(B"001", B"010", B"011") {
              /* MULH, MULHSU, MULHU */
              when(outputSign) {
                ansPayload.regDestValue := unsignedToSigned(multiplier.io.P)(63 downto 32).asBits
              } otherwise {
                ansPayload.regDestValue := multiplier.io.P(63 downto 32).asBits
              }
            }
          }
          io.answer.valid := True
          goto(idle)
        }
        waitCounter := waitCounter - 1
      } otherwise {
        when(divider.io.m_axis_dout_tvalid) {
          when(!isRem) {
            when(outputSign) {
              ansPayload.regDestValue := unsignedToSigned(divider.io.m_axis_dout_tdata)(63 downto 32).asBits
            } otherwise {
              ansPayload.regDestValue := divider.io.m_axis_dout_tdata(63 downto 32).asBits
            }
          } otherwise {
            when(outputSign) {
              ansPayload.regDestValue := unsignedToSigned(divider.io.m_axis_dout_tdata)(31 downto 0).asBits
            } otherwise {
              ansPayload.regDestValue := divider.io.m_axis_dout_tdata(31 downto 0).asBits
            }
          }
          io.answer.valid := True
          goto(idle)
        }
      }
    }
  }
}
