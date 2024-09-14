package openrv300.pipeline

import openrv300.isa.MicroOp
import spinal.core._
import spinal.lib._
import payload.{DecodePayload, ExecMemPayload}
import control._

case class InstExec() extends Component {
  val io = new Bundle {
    val request = slave(Flow(DecodePayload()))
    val answer = master(Flow(ExecMemPayload()))
    val bypassWritePort = master(BypassWritePort())
  }

  val reqData = io.request.payload
  val ansPayload = Reg(ExecMemPayload())

  ansPayload.microOp := reqData.microOp
  ansPayload.instPc := reqData.instPc
  ansPayload.instruction := reqData.instruction
  ansPayload.function0 := reqData.function0
  ansPayload.function1 := reqData.function1
  ansPayload.regSource0 := reqData.regSource0
  ansPayload.regSource1 := reqData.regSource1
  ansPayload.regDest := reqData.regDest
  ansPayload.imm := reqData.imm
  ansPayload.sextImm := reqData.sextImm

  ansPayload.writeRegDest := False
  ansPayload.regDestValue := B"32'd0"

  ansPayload.takeJump := False
  ansPayload.jumpPc := U"32'd0"

  val bypassWPort = Reg(BypassWritePort())
  io.bypassWritePort := bypassWPort

  bypassWPort.whichReg := U"5'd0"
  bypassWPort.finished := False
  bypassWPort.regValue := B"32'd0"

  def insertBypass(): Unit = {
    bypassWPort.whichReg := ansPayload.writeRegDest
    bypassWPort.regValue := ansPayload.regDestValue
    bypassWPort.finished := True
  }

  io.answer.setIdle()

  when(io.request.valid) {
    io.answer.push(ansPayload)

    switch(reqData.microOp) {
      is(MicroOp.LUI) {
        ansPayload.writeRegDest := True
        ansPayload.regDestValue := Cat(reqData.imm, B"12'd0")
        insertBypass()
      }
      is(MicroOp.AUIPC) {
        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (reqData.instPc + Cat(reqData.imm, B"12'd0").asUInt).asBits
        insertBypass()
      }
      is(MicroOp.JAL) {
        ansPayload.takeJump := True
        ansPayload.jumpPc := reqData.instPc + reqData.sextImm.asUInt

        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (reqData.instPc + U"32'd4").asBits
      }
      is(MicroOp.JALR) {
        ansPayload.takeJump := True
        ansPayload.jumpPc := reqData.regSource0.value.asUInt + reqData.sextImm.asUInt

        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (reqData.instPc + U"32'd4").asBits
      }
      is(MicroOp.BRANCH) {
        switch(reqData.function0) {
          is(B"000") {
            /* BEQ */
            ansPayload.takeJump := reqData.regSource0.value === reqData.regSource1.value
          }
          is(B"001") {
            /* BNE */
            ansPayload.takeJump := reqData.regSource0.value =/= reqData.regSource1.value
          }
          is(B"100") {
            /* BLT */
            ansPayload.takeJump := reqData.regSource0.value.asSInt < reqData.regSource1.value.asSInt
          }
          is(B"101") {
            /* BGE */
            ansPayload.takeJump := reqData.regSource0.value.asSInt >= reqData.regSource1.value.asSInt
          }
          is(B"110") {
            /* BLTU */
            ansPayload.takeJump := reqData.regSource0.value.asUInt < reqData.regSource1.value.asUInt
          }
          is(B"111") {
            /* BGEU */
            ansPayload.takeJump := reqData.regSource0.value.asUInt >= reqData.regSource1.value.asUInt
          }
        }
        default {
          /* TODO: illegal Inst. */
        }

        ansPayload.jumpPc := reqData.instPc + reqData.sextImm.asUInt
      }
      is(MicroOp.LOAD, MicroOp.STORE) {
        ansPayload.writeRegDest := reqData.microOp === MicroOp.LOAD
        ansPayload.memoryAddress := reqData.regSource0.value.asUInt + reqData.sextImm.asUInt
      }
      is(MicroOp.ARITH_BINARY_IMM) {
        ansPayload.writeRegDest := True
        switch(reqData.function0) {
          is(B"000") {
            /* ADDI */
            ansPayload.regDestValue := (reqData.regSource0.value.asUInt + reqData.sextImm.asUInt).asBits
          }
          is(B"010") {
            /* SLTI */
            ansPayload.regDestValue := Cat(B"31'd0",
              reqData.regSource0.value.asSInt < reqData.sextImm)
          }
          is(B"011") {
            /* SLTIU */
            ansPayload.regDestValue := Cat(B"31'd0",
              reqData.regSource0.value.asUInt < reqData.sextImm.asUInt)
          }
          is(B"100") {
            /* XORI */
            ansPayload.regDestValue := (reqData.regSource0.value.asUInt ^ reqData.sextImm.asUInt).asBits
          }
          is(B"110") {
            /* ORI */
            ansPayload.regDestValue := (reqData.regSource0.value.asUInt | reqData.sextImm.asUInt).asBits
          }
          is(B"111") {
            /* ANDI */
            ansPayload.regDestValue := (reqData.regSource0.value.asUInt & reqData.sextImm.asUInt).asBits
          }
          default {
            /* TODO: illegal Inst. */
          }
        }
        insertBypass()
      }
      is(MicroOp.ARITH_SLL_IMM) {
        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (reqData.regSource0.value.asUInt |<< reqData.imm.resize(5).asUInt).asBits
        insertBypass()
      }
      is(MicroOp.ARITH_SRL_IMM) {
        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (reqData.regSource0.value.asUInt |>> reqData.imm.resize(5).asUInt).asBits
        insertBypass()
      }
      is(MicroOp.ARITH_SRA_IMM) {
        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (reqData.regSource0.value.asSInt >> reqData.imm.resize(5).asUInt).asBits
        insertBypass()
      }
      is(MicroOp.ARITH_BINARY) {
        ansPayload.writeRegDest := True
        when(reqData.function1(0) === False) {
          switch(reqData.function0) {
            is(B"000") {
              /* ADD */
              ansPayload.regDestValue := (reqData.regSource0.value.asUInt + reqData.regSource1.value.asUInt).asBits
            }
            is(B"010") {
              /* SLT */
              ansPayload.regDestValue := Cat(B"31'd0",
                reqData.regSource0.value.asSInt < reqData.regSource1.value.asSInt).asBits
            }
            is(B"011") {
              /* SLTU */
              ansPayload.regDestValue := Cat(B"31'd0",
                reqData.regSource0.value.asUInt < reqData.regSource1.value.asUInt).asBits
            }
            is(B"100") {
              /* XOR */
              ansPayload.regDestValue := (reqData.regSource0.value.asUInt ^ reqData.regSource1.value.asUInt).asBits
            }
            is(B"110") {
              /* OR */
              ansPayload.regDestValue := (reqData.regSource0.value.asUInt | reqData.regSource1.value.asUInt).asBits
            }
            is(B"111") {
              /* AND */
              ansPayload.regDestValue := (reqData.regSource0.value.asUInt & reqData.regSource1.value.asUInt).asBits
            }
            default {
              /* TODO: illegal Inst. */
            }
          }
        } otherwise {
          when(reqData.function0 === B"000") {
            /* SUB */
            ansPayload.regDestValue := (reqData.regSource0.value.asUInt + reqData.regSource1.value.asUInt).asBits
          } otherwise {
            /* TODO: illegal Inst. */
          }
        }
        insertBypass()
      }
      is(MicroOp.ARITH_SLL) {
        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (reqData.regSource0.value.asUInt |<< reqData.regSource1.value.asUInt.resize(5)).asBits
        insertBypass()
      }
      is(MicroOp.ARITH_SRL) {
        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (reqData.regSource0.value.asUInt |>> reqData.regSource1.value.asUInt.resize(5)).asBits
        insertBypass()
      }
      is(MicroOp.ARITH_SRA) {
        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (reqData.regSource0.value.asSInt >> reqData.regSource1.value.asUInt.resize(5)).asBits
        insertBypass()
      }
      default {
        /* TODO: illegal Inst. */
      }
    }
  }

}
