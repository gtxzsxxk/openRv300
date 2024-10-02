package openrv300.pipeline

import spinal.core._
import spinal.lib._
import payload.{DecodePayload, FetchPayload, RegisterSourceBundle}
import openrv300.isa._
import openrv300.pipeline.control.BypassReadPort
import openrv300.pipeline.fifo.FetchBufferElement
import openrv300.regfile.{GPRs, GPRsReadPort}

case class InstDecode() extends Component {
  val io = new Bundle {
    val fetchBufferPop = out port Bool()
    val fetchBufferHead = in port FetchBufferElement()
    val answer = master(Flow(DecodePayload()))
    val regReadPorts = Vec.fill(2)(master(GPRsReadPort()))
    val bypassReadPorts = Vec.fill(2)(master(BypassReadPort()))
    /* 纯组合逻辑 */
    val execRegisters = out port Vec.fill(2)(RegisterSourceBundle())
    val waitForSrcReg = out port Bool()
    val execNeedStall = in port Bool()
    val execNeedStallInst = in port Bits(32 bits)
    val takeJump = in port Bool()
  }

  /* 此时源寄存器不再随着ans payload寄存，所以需要使用寄存器 */
  val registerSourceGPRs = Reg(Vec.fill(2)(RegisterSourceBundle()))

  for (idx <- 0 until 2) {
    io.regReadPorts(idx).readEnable := False
    io.regReadPorts(idx).readAddr := U"5'd0"
  }

  def genRegSourceBundle(instruction: Bits, msb: Int, lsb: Int, port: Int): Unit = {
    val bundle = RegisterSourceBundle()
    bundle.which := instruction(msb downto lsb).asUInt

    bundle.value := io.regReadPorts(port).readData
    io.regReadPorts(port).readEnable := True
    io.regReadPorts(port).readAddr := bundle.which

    registerSourceGPRs(port) := bundle
  }

  val reqData = FetchPayload()
  reqData.pcAddr := 0
  reqData.instruction := 0
  val reqDataValid = Bool()
  reqDataValid := False
  val reqDataValidReg = RegNext(reqDataValid) init (False)
  val takeJumpReg = RegNext(io.takeJump)
  val ansPayload = Reg(DecodePayload())

  /* 流水线停顿重放 */
  val lastRequest = Reg(FetchPayload())
  val needRedoLastRequest = Reg(Bool())
  io.fetchBufferPop := False

  when(io.execNeedStall && io.execNeedStallInst =/= lastRequest.instruction) {
    reqDataValid := False
    /* fetchBuffer 中的数据已经被pop了，因此暂存在lastRequest中 */
    needRedoLastRequest := True
  } elsewhen (io.waitForSrcReg || needRedoLastRequest) {
    io.fetchBufferPop := False
    reqData := lastRequest
    reqDataValid := True

    needRedoLastRequest := False
  } otherwise {
    reqDataValid := False
    needRedoLastRequest := False
    when(io.fetchBufferHead.valid) {
      io.fetchBufferPop := True
      lastRequest := io.fetchBufferHead.payload
      reqData := io.fetchBufferHead.payload
      reqDataValid := True
    }
  }

  //  ansPayload.microOp := B"7'd0"
  ansPayload.instPc := reqData.pcAddr
  ansPayload.instruction := reqData.instruction
  ansPayload.function0 := B"3'd0"
  ansPayload.function1 := B"7'd0"
  ansPayload.regDest := U"5'd0"
  ansPayload.imm := B"20'd0"
  ansPayload.sextImm := S"32'd0"

  def NOP(microOp: Bits = MicroOp.ARITH_BINARY_IMM): Unit = {
    val tmpBundle = RegisterSourceBundle()
    ansPayload.microOp := microOp
    ansPayload.function0 := B"000"

    ansPayload.regDest := U"5'd0"
    ansPayload.imm := B"12'd0".resized

    tmpBundle.which := U"5'd0"
    tmpBundle.value := B"0".resized
    tmpBundle.pending := False

    /* 此时寄存器数据不会随着ans payload寄存，所以需要另开一个寄存器 */
    /* 但是也不能直接覆盖到execRegisters上，否则会覆盖前一段的组合逻辑 */
    registerSourceGPRs(0) := tmpBundle
  }

  /* 从旁路读取寄存器数据，纯组合逻辑 */
  for (idx <- 0 until 2) {
    io.execRegisters(idx).pending := Mux(io.bypassReadPorts(idx).isBypassing, io.bypassReadPorts(idx).pending, False)

    io.bypassReadPorts(idx).whichReg := registerSourceGPRs(idx).which
  }

  io.waitForSrcReg := False

  def checkStall(regSrcIdx: Int): Unit = {
    when(io.execRegisters(regSrcIdx).pending) {
      io.waitForSrcReg := True
    }
  }

  for (port <- 0 until 2) {
    when(io.bypassReadPorts(port).isBypassing) {
      io.execRegisters(port).value := io.bypassReadPorts(port).regValue
    } otherwise {
      io.execRegisters(port).value := registerSourceGPRs(port).value
    }

    io.execRegisters(port).which := registerSourceGPRs(port).which
  }

  io.answer.payload := ansPayload
  io.answer.valid := reqDataValidReg & !takeJumpReg

  switch(ansPayload.instruction) {
    is(RV32I.JALR) {
      checkStall(0)
    }
    is(RV32I.BEQ, RV32I.BNE, RV32I.BLT, RV32I.BGE, RV32I.BLTU, RV32I.BGEU) {
      checkStall(0)
      checkStall(1)
    }
    is(RV32I.LB, RV32I.LH, RV32I.LW, RV32I.LBU, RV32I.LHU) {
      checkStall(0)
    }
    is(RV32I.SB, RV32I.SH, RV32I.SW) {
      checkStall(0)
      checkStall(1)
    }
    is(RV32I.ADDI, RV32I.SLTI, RV32I.SLTIU, RV32I.XORI, RV32I.ORI, RV32I.ANDI) {
      checkStall(0)
    }
    is(RV32I.SLLI, RV32I.SRLI, RV32I.SRAI) {
      checkStall(0)
    }
    is(RV32I.ADD, RV32I.SUB, RV32I.SLL, RV32I.SLT, RV32I.SLTU,
      RV32I.XOR, RV32I.SRL, RV32I.SRA, RV32I.OR, RV32I.AND,
      RV32M.MUL, RV32M.MULH, RV32M.MULHU, RV32M.MULHSU,
      RV32M.DIV, RV32M.DIVU, RV32M.REM, RV32M.REMU) {
      checkStall(0)
      checkStall(1)
    }
  }

  switch(reqData.instruction) {
    is(RV32I.LUI, RV32I.AUIPC) {
      ansPayload.microOp := Mux(reqData.instruction === RV32I.LUI, MicroOp.LUI, MicroOp.AUIPC)
      ansPayload.regDest := reqData.instruction(11 downto 7).asUInt
      ansPayload.imm := reqData.instruction(31 downto 12)
    }
    is(RV32I.JAL) {
      ansPayload.microOp := MicroOp.JAL
      ansPayload.regDest := reqData.instruction(11 downto 7).asUInt
      ansPayload.sextImm :=
        Cat(reqData.instruction(31), reqData.instruction(19 downto 12),
          reqData.instruction(20), reqData.instruction(30 downto 21), B"0").asSInt.resize(32)
    }
    is(RV32I.JALR) {
      ansPayload.microOp := MicroOp.JALR
      ansPayload.regDest := reqData.instruction(11 downto 7).asUInt
      ansPayload.sextImm := reqData.instruction(31 downto 20).asSInt.resize(32)
      genRegSourceBundle(reqData.instruction, 19, 15, 0)
    }
    is(RV32I.BEQ, RV32I.BNE, RV32I.BLT, RV32I.BGE, RV32I.BLTU, RV32I.BGEU) {
      ansPayload.microOp := MicroOp.BRANCH
      ansPayload.function0 := reqData.instruction(14 downto 12)
      ansPayload.sextImm :=
        Cat(reqData.instruction(31), reqData.instruction(7),
          reqData.instruction(30 downto 25),
          reqData.instruction(11 downto 8), B"0").asSInt.resize(32)
      genRegSourceBundle(reqData.instruction, 19, 15, 0)
      genRegSourceBundle(reqData.instruction, 24, 20, 1)
    }
    is(RV32I.LB, RV32I.LH, RV32I.LW, RV32I.LBU, RV32I.LHU) {
      ansPayload.microOp := MicroOp.LOAD
      ansPayload.function0 := reqData.instruction(14 downto 12)
      ansPayload.sextImm := reqData.instruction(31 downto 20).asSInt.resize(32)

      genRegSourceBundle(reqData.instruction, 19, 15, 0)
      ansPayload.regDest := reqData.instruction(11 downto 7).asUInt
    }
    is(RV32I.SB, RV32I.SH, RV32I.SW) {
      ansPayload.microOp := MicroOp.STORE
      ansPayload.function0 := reqData.instruction(14 downto 12)
      ansPayload.sextImm :=
        Cat(reqData.instruction(31 downto 25),
          reqData.instruction(11 downto 7)).asSInt.resize(32)

      genRegSourceBundle(reqData.instruction, 19, 15, 0)
      genRegSourceBundle(reqData.instruction, 24, 20, 1)
    }
    is(RV32I.ADDI, RV32I.SLTI, RV32I.SLTIU, RV32I.XORI, RV32I.ORI, RV32I.ANDI) {
      ansPayload.microOp := MicroOp.ARITH_BINARY_IMM
      ansPayload.function0 := reqData.instruction(14 downto 12)

      ansPayload.regDest := reqData.instruction(11 downto 7).asUInt
      genRegSourceBundle(reqData.instruction, 19, 15, 0)
      ansPayload.sextImm := reqData.instruction(31 downto 20).asSInt.resize(32)
    }
    is(RV32I.SLLI, RV32I.SRLI, RV32I.SRAI) {
      when(reqData.instruction === RV32I.SLLI) {
        ansPayload.microOp := MicroOp.ARITH_SLL_IMM
      } elsewhen (reqData.instruction === RV32I.SRLI) {
        ansPayload.microOp := MicroOp.ARITH_SRL_IMM
      } otherwise {
        ansPayload.microOp := MicroOp.ARITH_SRA_IMM
      }

      ansPayload.regDest := reqData.instruction(11 downto 7).asUInt
      genRegSourceBundle(reqData.instruction, 19, 15, 0)
      ansPayload.imm := reqData.instruction(24 downto 20).resized
    }
    is(RV32I.ADD, RV32I.SUB, RV32I.SLL, RV32I.SLT, RV32I.SLTU,
      RV32I.XOR, RV32I.SRL, RV32I.SRA, RV32I.OR, RV32I.AND,
      RV32M.MUL, RV32M.MULH, RV32M.MULHU, RV32M.MULHSU,
      RV32M.DIV, RV32M.DIVU, RV32M.REM, RV32M.REMU) {
      when(reqData.instruction === RV32I.SLL) {
        ansPayload.microOp := MicroOp.ARITH_SLL
      } elsewhen (reqData.instruction === RV32I.SRL) {
        ansPayload.microOp := MicroOp.ARITH_SRL
      } elsewhen (reqData.instruction === RV32I.SRA) {
        ansPayload.microOp := MicroOp.ARITH_SRA
      } otherwise {
        ansPayload.microOp := MicroOp.ARITH_BINARY
      }

      ansPayload.regDest := reqData.instruction(11 downto 7).asUInt
      genRegSourceBundle(reqData.instruction, 19, 15, 0)
      genRegSourceBundle(reqData.instruction, 24, 20, 1)

      ansPayload.function0 := reqData.instruction(14 downto 12)
      ansPayload.function1 := reqData.instruction(31 downto 25)
    }
    is(RV32I.FENCE, RV32I.FENCE_TSO, RV32I.PAUSE, RV32I.EBREAK) {
      /* decode as nop */
      NOP()
    }
    is(RV32I.ECALL) {
      /* TODO: raise an exception */
      NOP(MicroOp.ECALL)
    }
    default {
      /* TODO: raise an exception */
      NOP()
    }
  }

}
