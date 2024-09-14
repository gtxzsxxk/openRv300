package openrv300.pipeline

import spinal.core._
import spinal.lib._
import payload.{DecodePayload, FetchPayload, RegisterSourceBundle}
import openrv300.isa._
import openrv300.regfile.{GPRs, GPRsReadPort}

case class InstDecode() extends Component {
  val io = new Bundle {
    val request = slave(Flow(FetchPayload()))
    val answer = master(Flow(DecodePayload()))
    val regReadPorts = Vec.fill(2)(master(GPRsReadPort()))
  }

  for(idx <- 0 until 2) {
    io.regReadPorts(idx).readEnable := False
    io.regReadPorts(idx).readAddr := U"5'd0"
  }

  def genRegSourceBundle(instruction: Bits, msb: Int, lsb: Int, port: Int): RegisterSourceBundle = {
    val bundle = RegisterSourceBundle()
    bundle.which := instruction(msb downto lsb).asUInt

    bundle.value := io.regReadPorts(port).readData
    io.regReadPorts(port).readEnable := True
    io.regReadPorts(port).readAddr := bundle.which

    bundle
  }

  def regNotUsed(port: Int): Unit = {
    io.regReadPorts(port).readEnable := False
  }

  def regBothNotUsed(): Unit = {
    regNotUsed(0)
    regNotUsed(1)
  }

  def NOP(): Unit = {
    ansPayload.microOp := MicroOp.ARITH_BINARY_IMM
    ansPayload.function0 := B"000"

    ansPayload.regDest := U"5'd0"
    ansPayload.regSource0 := genRegSourceBundle(B"32'd0", 19, 15, 0)
    ansPayload.imm := B"12'd0".resized

    regNotUsed(1)
  }

  val reqData = io.request.payload
  val ansPayload = Reg(DecodePayload())

  ansPayload.microOp := B"7'd0"
  ansPayload.instPc := reqData.pcAddr
  ansPayload.instruction := reqData.instruction
  ansPayload.function0 := B"3'd0"
  ansPayload.function1 := B"7'd0"
  ansPayload.regSource0.which := U"5'd0"
  ansPayload.regSource0.value := B"32'd0"
  ansPayload.regSource1.which := U"5'd0"
  ansPayload.regSource1.value := B"32'd0"
  ansPayload.regDest := U"5'd0"
  ansPayload.imm := B"20'd0"
  ansPayload.sextImm := S"32'd0"

  io.answer.setIdle()

  when(io.request.valid) {
    io.answer.push(ansPayload)

    switch(reqData.instruction) {
      is(RV32I.LUI, RV32I.AUIPC) {
        ansPayload.microOp := Mux(reqData.instruction === RV32I.LUI, MicroOp.LUI, MicroOp.AUIPC)
        ansPayload.regDest := reqData.instruction(11 downto 7).asUInt
        ansPayload.imm := reqData.instruction(31 downto 12)

        regBothNotUsed()
      }
      is(RV32I.JAL) {
        ansPayload.microOp := MicroOp.JAL
        ansPayload.regDest := reqData.instruction(11 downto 7).asUInt
        ansPayload.sextImm :=
          Cat(reqData.instruction(31), reqData.instruction(19 downto 12),
            reqData.instruction(20), reqData.instruction(30 downto 21), B"0").asSInt.resize(32)

        regBothNotUsed()
      }
      is(RV32I.JALR) {
        ansPayload.microOp := MicroOp.JALR
        ansPayload.regDest := reqData.instruction(11 downto 7).asUInt
        ansPayload.sextImm := reqData.instruction(31 downto 20).asSInt.resize(32)
        ansPayload.regSource0 := genRegSourceBundle(reqData.instruction, 19, 15, 0)

        regNotUsed(1)
      }
      is(RV32I.BEQ, RV32I.BNE, RV32I.BLT, RV32I.BGE, RV32I.BLTU, RV32I.BGEU) {
        ansPayload.microOp := MicroOp.BRANCH
        ansPayload.function0 := reqData.instruction(14 downto 12)
        ansPayload.sextImm :=
          Cat(reqData.instruction(31), reqData.instruction(7),
            reqData.instruction(30 downto 25),
            reqData.instruction(11 downto 8), B"0").asSInt.resize(32)
        ansPayload.regSource0 := genRegSourceBundle(reqData.instruction, 19, 15, 0)
        ansPayload.regSource1 := genRegSourceBundle(reqData.instruction, 24, 20, 1)
      }
      is(RV32I.LB, RV32I.LH, RV32I.LW, RV32I.LBU, RV32I.LHU) {
        ansPayload.microOp := MicroOp.LOAD
        ansPayload.function0 := reqData.instruction(14 downto 12)
        ansPayload.sextImm := reqData.instruction(31 downto 20).asSInt.resize(32)

        ansPayload.regSource0 := genRegSourceBundle(reqData.instruction, 19, 15, 0)
        ansPayload.regDest := reqData.instruction(11 downto 7).asUInt

        regNotUsed(1)
      }
      is(RV32I.SB, RV32I.SH, RV32I.SW) {
        ansPayload.microOp := MicroOp.STORE
        ansPayload.function0 := reqData.instruction(14 downto 12)
        ansPayload.sextImm :=
          Cat(reqData.instruction(31 downto 25),
            reqData.instruction(11 downto 7)).asSInt.resize(32)

        ansPayload.regSource0 := genRegSourceBundle(reqData.instruction, 19, 15, 0)
        ansPayload.regSource1 := genRegSourceBundle(reqData.instruction, 24, 20, 1)
      }
      is(RV32I.ADDI, RV32I.SLTI, RV32I.SLTIU, RV32I.XORI, RV32I.ORI, RV32I.ANDI) {
        ansPayload.microOp := MicroOp.ARITH_BINARY_IMM
        ansPayload.function0 := reqData.instruction(14 downto 12)

        ansPayload.regDest := reqData.instruction(11 downto 7).asUInt
        ansPayload.regSource0 := genRegSourceBundle(reqData.instruction, 19, 15, 0)
        ansPayload.sextImm := reqData.instruction(31 downto 20).asSInt.resize(32)

        regNotUsed(1)
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
        ansPayload.regSource0 := genRegSourceBundle(reqData.instruction, 19, 15, 0)
        ansPayload.imm := reqData.instruction(24 downto 20).resized

        regNotUsed(1)
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
        ansPayload.regSource0 := genRegSourceBundle(reqData.instruction, 19, 15, 0)
        ansPayload.regSource1 := genRegSourceBundle(reqData.instruction, 24, 20, 1)

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
}
