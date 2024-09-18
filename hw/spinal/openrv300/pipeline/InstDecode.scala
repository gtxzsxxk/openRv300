package openrv300.pipeline

import spinal.core._
import spinal.lib._
import payload.{DecodePayload, FetchPayload, RegisterSourceBundle}
import openrv300.isa._
import openrv300.pipeline.control.BypassReadPort
import openrv300.regfile.{GPRs, GPRsReadPort}

case class InstDecode() extends Component {
  val io = new Bundle {
    val request = slave(Flow(FetchPayload()))
    val answer = master(Flow(DecodePayload()))
    val regReadPorts = Vec.fill(2)(master(GPRsReadPort()))
    val bypassReadPorts = Vec.fill(2)(master(BypassReadPort()))
    /* 纯组合逻辑 */
    val execRegisters = out port Vec.fill(2)(RegisterSourceBundle())
    val stall = out port Bool()
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

  def NOP(): Unit = {
    val tmpBundle = RegisterSourceBundle()
    ansPayload.microOp := MicroOp.ARITH_BINARY_IMM
    ansPayload.function0 := B"000"

    ansPayload.regDest := U"5'd0"
    ansPayload.imm := B"12'd0".resized

    tmpBundle.which := U"5'd0"
    tmpBundle.pending := False

    /* 此时寄存器数据不会随着ans payload寄存，所以需要另开一个寄存器 */
    /* 但是也不能直接覆盖到execRegisters上，否则会覆盖前一段的组合逻辑 */
    registerSourceGPRs(0) := tmpBundle
  }

  val reqData = FetchPayload()
  val ansPayload = Reg(DecodePayload())

  /* 流水线停顿重放 */
  val justReset = Reg(Bool()) init (True)
  val lastRequest = RegNext(io.request.payload)
  /* 上一条指令译码后，源操作数全部满足，才开始本条指令译码，否则重放上条指令进行译码 */
  when(justReset) {
    justReset := False
    reqData.instruction := B"32'h00000013"
    reqData.pcAddr := U"32'd0"
  } otherwise {
    when(!io.stall) {
      reqData := io.request.payload
    } otherwise {
      reqData := lastRequest
    }
  }

  ansPayload.microOp := B"7'd0"
  ansPayload.instPc := reqData.pcAddr
  ansPayload.instruction := reqData.instruction
  ansPayload.function0 := B"3'd0"
  ansPayload.function1 := B"7'd0"
  ansPayload.regDest := U"5'd0"
  ansPayload.imm := B"20'd0"
  ansPayload.sextImm := S"32'd0"

  /* 从旁路读取寄存器数据，纯组合逻辑 */
  for (idx <- 0 until 2) {
    io.execRegisters(idx).pending := Mux(io.bypassReadPorts(idx).isBypassing, io.bypassReadPorts(idx).pending, False)

    io.bypassReadPorts(idx).whichReg := registerSourceGPRs(idx).which
  }

  io.stall := False

  def checkStall(regSrcIdx: Int): Unit = {
    when(io.execRegisters(regSrcIdx).pending) {
      io.stall := True
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

  io.answer.push(ansPayload)

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
    is(RV32I.ADDI, RV32I.SLTI, RV32I.SLTIU, RV32I.XORI, RV32I.ORI, RV32I.ANDI) {
      checkStall(0)
    }
    is(RV32I.SLLI, RV32I.SRLI, RV32I.SRAI) {
      checkStall(0)
    }
    is(RV32I.ADD, RV32I.SUB, RV32I.SLL, RV32I.SLT, RV32I.SLTU, RV32I.XOR, RV32I.SRL, RV32I.SRA, RV32I.OR, RV32I.AND) {
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

      reqData.instruction(11 downto 7).asUInt
      genRegSourceBundle(reqData.instruction, 19, 15, 0)
      ansPayload.imm := reqData.instruction(24 downto 20).resized
    }
    is(RV32I.ADD, RV32I.SUB, RV32I.SLL, RV32I.SLT, RV32I.SLTU, RV32I.XOR, RV32I.SRL, RV32I.SRA, RV32I.OR, RV32I.AND) {
      when(reqData.instruction =/= RV32I.SLL && reqData.instruction =/= RV32I.SRL && reqData.instruction =/= RV32I.SRA) {
        ansPayload.microOp := MicroOp.ARITH_BINARY
      } elsewhen (reqData.instruction === RV32I.SLL) {
        ansPayload.microOp := MicroOp.ARITH_SLL
      } elsewhen (reqData.instruction === RV32I.SRL) {
        ansPayload.microOp := MicroOp.ARITH_SRL
      } elsewhen (reqData.instruction === RV32I.SRA) {
        ansPayload.microOp := MicroOp.ARITH_SRA
      }

      ansPayload.regDest := reqData.instruction(11 downto 7).asUInt
      genRegSourceBundle(reqData.instruction, 19, 15, 0)
      genRegSourceBundle(reqData.instruction, 24, 20, 1)

      ansPayload.function0 := reqData.instruction(14 downto 12)
      ansPayload.function1 := reqData.instruction(30).asBits.resized
    }
    is(RV32I.FENCE, RV32I.FENCE_TSO, RV32I.PAUSE, RV32I.EBREAK) {
      /* decode as nop */
      NOP()
    }
    is(RV32I.ECALL) {
      /* TODO: raise an exception */
      NOP()
    }
    default {
      /* TODO: raise an exception */
      NOP()
    }
  }

}
