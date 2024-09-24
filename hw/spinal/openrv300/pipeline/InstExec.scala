package openrv300.pipeline

import openrv300.isa.MicroOp
import spinal.core._
import spinal.lib._
import payload.{DecodePayload, ExecMemPayload, RegisterSourceBundle}
import control._

case class InstExec() extends Component {
  val io = new Bundle {
    val request = slave(Flow(DecodePayload()))
    val answer = master(Flow(ExecMemPayload()))
    val bypassWritePort = master(BypassWritePort())
    val execRegisters = in port Vec.fill(2)(RegisterSourceBundle())
    val isStalling = in port Bool()
  }

  val reqData = io.request.payload
  val ansPayload = Reg(ExecMemPayload())

  ansPayload.microOp := reqData.microOp
  ansPayload.instPc := reqData.instPc
  ansPayload.instruction := reqData.instruction
  ansPayload.function0 := reqData.function0
  ansPayload.function1 := reqData.function1
  ansPayload.regDest := reqData.regDest
  ansPayload.registerSources := io.execRegisters
  ansPayload.imm := reqData.imm
  ansPayload.sextImm := reqData.sextImm

  ansPayload.writeRegDest := False
  ansPayload.regDestValue := B"32'd0"

  ansPayload.takeJump := False
  ansPayload.jumpPc := U"32'd0"

  val bypassWPort = BypassWritePort().noCombLoopCheck
  val bypassValueReady = Reg(Bool()) init (False)
  bypassValueReady := False
  io.bypassWritePort := bypassWPort
  bypassWPort.whichReg := U"5'd0"
  bypassWPort.finished := bypassValueReady
  bypassWPort.regValue := B"32'd0"

  def insertBypass(solvedThisStage: Boolean): Unit = {
    bypassWPort.whichReg := ansPayload.regDest
    bypassWPort.regValue := ansPayload.regDestValue
    bypassValueReady := Bool(solvedThisStage)
  }

  def registerSourceValues(idx: Int): Bits = {
    io.execRegisters(idx).value
  }

  def NOP(): Unit = {
    ansPayload.microOp := MicroOp.ARITH_BINARY_IMM
    ansPayload.writeRegDest := True
    ansPayload.regDest := U"5'd0"
    ansPayload.regDestValue := B"32'd0"
    insertBypass(true)
  }

  io.answer.push(ansPayload)

  when(io.isStalling || !io.request.valid || ansPayload.takeJump) {
    /* 译出NOP */
    NOP()
  } otherwise  {
    switch(reqData.microOp) {
      is(MicroOp.LUI) {
        ansPayload.writeRegDest := True
        ansPayload.regDestValue := Cat(reqData.imm, B"12'd0")
        insertBypass(true)
      }
      is(MicroOp.AUIPC) {
        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (reqData.instPc + Cat(reqData.imm, B"12'd0").asUInt).asBits
        insertBypass(true)
      }
      is(MicroOp.JAL) {
        ansPayload.takeJump := True
        ansPayload.jumpPc := reqData.instPc + reqData.sextImm.asUInt

        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (reqData.instPc + U"32'd4").asBits
      }
      is(MicroOp.JALR) {
        ansPayload.takeJump := True
        ansPayload.jumpPc := registerSourceValues(0).asUInt + reqData.sextImm.asUInt

        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (reqData.instPc + U"32'd4").asBits
      }
      is(MicroOp.BRANCH) {
        switch(reqData.function0) {
          is(B"000") {
            /* BEQ */
            ansPayload.takeJump := registerSourceValues(0) === registerSourceValues(1)
          }
          is(B"001") {
            /* BNE */
            ansPayload.takeJump := registerSourceValues(0) =/= registerSourceValues(1)
          }
          is(B"100") {
            /* BLT */
            ansPayload.takeJump := registerSourceValues(0).asSInt < registerSourceValues(1).asSInt
          }
          is(B"101") {
            /* BGE */
            ansPayload.takeJump := registerSourceValues(0).asSInt >= registerSourceValues(1).asSInt
          }
          is(B"110") {
            /* BLTU */
            ansPayload.takeJump := registerSourceValues(0).asUInt < registerSourceValues(1).asUInt
          }
          is(B"111") {
            /* BGEU */
            ansPayload.takeJump := registerSourceValues(0).asUInt >= registerSourceValues(1).asUInt
          }
        }
        default {
          /* TODO: illegal Inst. */
        }

        ansPayload.jumpPc := reqData.instPc + reqData.sextImm.asUInt

        NOP()
      }
      is(MicroOp.LOAD, MicroOp.STORE) {
        ansPayload.writeRegDest := reqData.microOp === MicroOp.LOAD
        ansPayload.memoryAddress := registerSourceValues(0).asUInt + reqData.sextImm.asUInt

        insertBypass(false)
      }
      is(MicroOp.ARITH_BINARY_IMM) {
        ansPayload.writeRegDest := True
        switch(reqData.function0) {
          is(B"000") {
            /* ADDI */
            ansPayload.regDestValue := (registerSourceValues(0).asUInt + reqData.sextImm.asUInt).asBits
          }
          is(B"010") {
            /* SLTI */
            ansPayload.regDestValue := Cat(B"31'd0",
              registerSourceValues(0).asSInt < reqData.sextImm)
          }
          is(B"011") {
            /* SLTIU */
            ansPayload.regDestValue := Cat(B"31'd0",
              registerSourceValues(0).asUInt < reqData.sextImm.asUInt)
          }
          is(B"100") {
            /* XORI */
            ansPayload.regDestValue := (registerSourceValues(0).asUInt ^ reqData.sextImm.asUInt).asBits
          }
          is(B"110") {
            /* ORI */
            ansPayload.regDestValue := (registerSourceValues(0).asUInt | reqData.sextImm.asUInt).asBits
          }
          is(B"111") {
            /* ANDI */
            ansPayload.regDestValue := (registerSourceValues(0).asUInt & reqData.sextImm.asUInt).asBits
          }
          default {
            /* TODO: illegal Inst. */
          }
        }
        insertBypass(true)
      }
      is(MicroOp.ARITH_SLL_IMM) {
        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (registerSourceValues(0).asUInt |<< reqData.imm.resize(5).asUInt).asBits
        insertBypass(true)
      }
      is(MicroOp.ARITH_SRL_IMM) {
        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (registerSourceValues(0).asUInt |>> reqData.imm.resize(5).asUInt).asBits
        insertBypass(true)
      }
      is(MicroOp.ARITH_SRA_IMM) {
        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (registerSourceValues(0).asSInt >> reqData.imm.resize(5).asUInt).asBits
        insertBypass(true)
      }
      is(MicroOp.ARITH_BINARY) {
        ansPayload.writeRegDest := True
        when(reqData.function1(0) === False) {
          switch(reqData.function0) {
            is(B"000") {
              /* ADD */
              ansPayload.regDestValue := (registerSourceValues(0).asUInt + registerSourceValues(1).asUInt).asBits
            }
            is(B"010") {
              /* SLT */
              ansPayload.regDestValue := Cat(B"31'd0",
                registerSourceValues(0).asSInt < registerSourceValues(1).asSInt).asBits
            }
            is(B"011") {
              /* SLTU */
              ansPayload.regDestValue := Cat(B"31'd0",
                registerSourceValues(0).asUInt < registerSourceValues(1).asUInt).asBits
            }
            is(B"100") {
              /* XOR */
              ansPayload.regDestValue := (registerSourceValues(0).asUInt ^ registerSourceValues(1).asUInt).asBits
            }
            is(B"110") {
              /* OR */
              ansPayload.regDestValue := (registerSourceValues(0).asUInt | registerSourceValues(1).asUInt).asBits
            }
            is(B"111") {
              /* AND */
              ansPayload.regDestValue := (registerSourceValues(0).asUInt & registerSourceValues(1).asUInt).asBits
            }
            default {
              /* TODO: illegal Inst. */
            }
          }
        } otherwise {
          when(reqData.function0 === B"000") {
            /* SUB */
            ansPayload.regDestValue := (registerSourceValues(0).asUInt + registerSourceValues(1).asUInt).asBits
          } otherwise {
            /* TODO: illegal Inst. */
          }
        }
        insertBypass(true)
      }
      is(MicroOp.ARITH_SLL) {
        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (registerSourceValues(0).asUInt |<< registerSourceValues(1).asUInt.resize(5)).asBits
        insertBypass(true)
      }
      is(MicroOp.ARITH_SRL) {
        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (registerSourceValues(0).asUInt |>> registerSourceValues(1).asUInt.resize(5)).asBits
        insertBypass(true)
      }
      is(MicroOp.ARITH_SRA) {
        ansPayload.writeRegDest := True
        ansPayload.regDestValue := (registerSourceValues(0).asSInt >> registerSourceValues(1).asUInt.resize(5)).asBits
        insertBypass(true)
      }
      default {
        /* TODO: illegal Inst. */
      }
    }
  }

}
