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
    val bypassReadPorts = Vec.fill(2)(master(BypassReadPort()))
    val bypassCheckPorts = Vec.fill(2)(master(BypassCheckPort()))
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
  val bypassWPort = BypassWritePort().noCombLoopCheck
  val bypassValueReady = Reg(Bool()) init (False)
  io.bypassWritePort := bypassWPort

  bypassWPort.whichReg := U"5'd0"
  bypassWPort.finished := bypassValueReady
  bypassWPort.regValue := B"32'd0"

  def insertBypass(solvedThisStage: Boolean): Unit = {
    bypassWPort.whichReg := ansPayload.regDest
    bypassWPort.regValue := ansPayload.regDestValue
    bypassValueReady := Bool(solvedThisStage)
  }

  for (idx <- 0 until 2) {
    io.bypassReadPorts(idx).whichReg := U"5'd0"
    io.bypassReadPorts(idx).readEnable := False

    io.bypassCheckPorts(idx).whichReg := U"5'd0"
    io.bypassCheckPorts(idx).checkEnable := False
  }

  def checkRegSource(which: UInt, port: Int): Bool = {
    io.bypassCheckPorts(port).whichReg := which
    io.bypassCheckPorts(port).checkEnable := True
    when(io.bypassCheckPorts(port).pending) {
      /* Stall takes happen */
      io.answer.setIdle()
    }
    !io.bypassCheckPorts(port).pending
  }

  def readRegSourceValue(which: UInt, port: Int): Bits = {
    io.bypassReadPorts(port).whichReg := which
    io.bypassReadPorts(port).readEnable := True
    io.bypassReadPorts(port).regValue
  }

  io.answer.setIdle()

  when(io.request.valid) {
    io.answer.push(ansPayload)

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
        when(checkRegSource(reqData.regSource0.which, 0)) {
          ansPayload.takeJump := True
          ansPayload.jumpPc := readRegSourceValue(reqData.regSource0.which, 0).asUInt + reqData.sextImm.asUInt

          ansPayload.writeRegDest := True
          ansPayload.regDestValue := (reqData.instPc + U"32'd4").asBits
        }
      }
      is(MicroOp.BRANCH) {
        when(checkRegSource(reqData.regSource0.which, 0) && checkRegSource(reqData.regSource1.which, 1)) {
          switch(reqData.function0) {
            is(B"000") {
              /* BEQ */
              ansPayload.takeJump := readRegSourceValue(reqData.regSource0.which, 0) === readRegSourceValue(reqData.regSource1.which, 1)
            }
            is(B"001") {
              /* BNE */
              ansPayload.takeJump := readRegSourceValue(reqData.regSource0.which, 0) =/= readRegSourceValue(reqData.regSource1.which, 1)
            }
            is(B"100") {
              /* BLT */
              ansPayload.takeJump := readRegSourceValue(reqData.regSource0.which, 0).asSInt < readRegSourceValue(reqData.regSource1.which, 1).asSInt
            }
            is(B"101") {
              /* BGE */
              ansPayload.takeJump := readRegSourceValue(reqData.regSource0.which, 0).asSInt >= readRegSourceValue(reqData.regSource1.which, 1).asSInt
            }
            is(B"110") {
              /* BLTU */
              ansPayload.takeJump := readRegSourceValue(reqData.regSource0.which, 0).asUInt < readRegSourceValue(reqData.regSource1.which, 1).asUInt
            }
            is(B"111") {
              /* BGEU */
              ansPayload.takeJump := readRegSourceValue(reqData.regSource0.which, 0).asUInt >= readRegSourceValue(reqData.regSource1.which, 1).asUInt
            }
          }
          default {
            /* TODO: illegal Inst. */
          }

          ansPayload.jumpPc := reqData.instPc + reqData.sextImm.asUInt
        }
      }
      is(MicroOp.LOAD, MicroOp.STORE) {
        when(checkRegSource(reqData.regSource0.which, 0)) {
          ansPayload.writeRegDest := reqData.microOp === MicroOp.LOAD
          ansPayload.memoryAddress := readRegSourceValue(reqData.regSource0.which, 0).asUInt + reqData.sextImm.asUInt

          insertBypass(false)
        }
      }
      is(MicroOp.ARITH_BINARY_IMM) {
        when(checkRegSource(reqData.regSource0.which, 0)) {
          ansPayload.writeRegDest := True
          switch(reqData.function0) {
            is(B"000") {
              /* ADDI */
              ansPayload.regDestValue := (readRegSourceValue(reqData.regSource0.which, 0).asUInt + reqData.sextImm.asUInt).asBits
            }
            is(B"010") {
              /* SLTI */
              ansPayload.regDestValue := Cat(B"31'd0",
                readRegSourceValue(reqData.regSource0.which, 0).asSInt < reqData.sextImm)
            }
            is(B"011") {
              /* SLTIU */
              ansPayload.regDestValue := Cat(B"31'd0",
                readRegSourceValue(reqData.regSource0.which, 0).asUInt < reqData.sextImm.asUInt)
            }
            is(B"100") {
              /* XORI */
              ansPayload.regDestValue := (readRegSourceValue(reqData.regSource0.which, 0).asUInt ^ reqData.sextImm.asUInt).asBits
            }
            is(B"110") {
              /* ORI */
              ansPayload.regDestValue := (readRegSourceValue(reqData.regSource0.which, 0).asUInt | reqData.sextImm.asUInt).asBits
            }
            is(B"111") {
              /* ANDI */
              ansPayload.regDestValue := (readRegSourceValue(reqData.regSource0.which, 0).asUInt & reqData.sextImm.asUInt).asBits
            }
            default {
              /* TODO: illegal Inst. */
            }
          }
          insertBypass(true)
        }
      }
      is(MicroOp.ARITH_SLL_IMM) {
        when(checkRegSource(reqData.regSource0.which, 0)) {
          ansPayload.writeRegDest := True
          ansPayload.regDestValue := (readRegSourceValue(reqData.regSource0.which, 0).asUInt |<< reqData.imm.resize(5).asUInt).asBits
          insertBypass(true)
        }
      }
      is(MicroOp.ARITH_SRL_IMM) {
        when(checkRegSource(reqData.regSource0.which, 0)) {
          ansPayload.writeRegDest := True
          ansPayload.regDestValue := (readRegSourceValue(reqData.regSource0.which, 0).asUInt |>> reqData.imm.resize(5).asUInt).asBits
          insertBypass(true)
        }
      }
      is(MicroOp.ARITH_SRA_IMM) {
        when(checkRegSource(reqData.regSource0.which, 0)) {
          ansPayload.writeRegDest := True
          ansPayload.regDestValue := (readRegSourceValue(reqData.regSource0.which, 0).asSInt >> reqData.imm.resize(5).asUInt).asBits
          insertBypass(true)
        }
      }
      is(MicroOp.ARITH_BINARY) {
        when(checkRegSource(reqData.regSource0.which, 0) && checkRegSource(reqData.regSource1.which, 1)) {
          ansPayload.writeRegDest := True
          when(reqData.function1(0) === False) {
            switch(reqData.function0) {
              is(B"000") {
                /* ADD */
                ansPayload.regDestValue := (readRegSourceValue(reqData.regSource0.which, 0).asUInt + readRegSourceValue(reqData.regSource1.which, 1).asUInt).asBits
              }
              is(B"010") {
                /* SLT */
                ansPayload.regDestValue := Cat(B"31'd0",
                  readRegSourceValue(reqData.regSource0.which, 0).asSInt < readRegSourceValue(reqData.regSource1.which, 1).asSInt).asBits
              }
              is(B"011") {
                /* SLTU */
                ansPayload.regDestValue := Cat(B"31'd0",
                  readRegSourceValue(reqData.regSource0.which, 0).asUInt < readRegSourceValue(reqData.regSource1.which, 1).asUInt).asBits
              }
              is(B"100") {
                /* XOR */
                ansPayload.regDestValue := (readRegSourceValue(reqData.regSource0.which, 0).asUInt ^ readRegSourceValue(reqData.regSource1.which, 1).asUInt).asBits
              }
              is(B"110") {
                /* OR */
                ansPayload.regDestValue := (readRegSourceValue(reqData.regSource0.which, 0).asUInt | readRegSourceValue(reqData.regSource1.which, 1).asUInt).asBits
              }
              is(B"111") {
                /* AND */
                ansPayload.regDestValue := (readRegSourceValue(reqData.regSource0.which, 0).asUInt & readRegSourceValue(reqData.regSource1.which, 1).asUInt).asBits
              }
              default {
                /* TODO: illegal Inst. */
              }
            }
          } otherwise {
            when(reqData.function0 === B"000") {
              /* SUB */
              ansPayload.regDestValue := (readRegSourceValue(reqData.regSource0.which, 0).asUInt + readRegSourceValue(reqData.regSource1.which, 1).asUInt).asBits
            } otherwise {
              /* TODO: illegal Inst. */
            }
          }
          insertBypass(true)
        }
      }
      is(MicroOp.ARITH_SLL) {
        when(checkRegSource(reqData.regSource0.which, 0) && checkRegSource(reqData.regSource1.which, 1)) {
          ansPayload.writeRegDest := True
          ansPayload.regDestValue := (readRegSourceValue(reqData.regSource0.which, 0).asUInt |<< readRegSourceValue(reqData.regSource1.which, 1).asUInt.resize(5)).asBits
          insertBypass(true)
        }
      }
      is(MicroOp.ARITH_SRL) {
        when(checkRegSource(reqData.regSource0.which, 0) && checkRegSource(reqData.regSource1.which, 1)) {
          ansPayload.writeRegDest := True
          ansPayload.regDestValue := (readRegSourceValue(reqData.regSource0.which, 0).asUInt |>> readRegSourceValue(reqData.regSource1.which, 1).asUInt.resize(5)).asBits
          insertBypass(true)
        }
      }
      is(MicroOp.ARITH_SRA) {
        when(checkRegSource(reqData.regSource0.which, 0) && checkRegSource(reqData.regSource1.which, 1)) {
          ansPayload.writeRegDest := True
          ansPayload.regDestValue := (readRegSourceValue(reqData.regSource0.which, 0).asSInt >> readRegSourceValue(reqData.regSource1.which, 1).asUInt.resize(5)).asBits
          insertBypass(true)
        }
      }
      default {
        /* TODO: illegal Inst. */
      }
    }
  }

}
