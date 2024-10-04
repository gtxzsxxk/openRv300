package openrv300.pipeline

import openrv300.cache.CacheCorePort
import openrv300.isa.MicroOp
import openrv300.pipeline.control.BypassWritePort
import openrv300.pipeline.payload._
import openrv300.privilege.CSRPort
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class MemAccess() extends Component {
  val io = new Bundle {
    val request = slave(Flow(ExecMemPayload()))
    val answer = master(Flow(ExecMemPayload()))
    val bypassWritePort = master(BypassWritePort())
    val dCachePort = master(CacheCorePort())
    val dCacheMiss = out port Bool()

    val csrPort = master(CSRPort())
  }

  val reqData = io.request.payload
  val ansPayload = Reg(ExecMemPayload())
  val ansValid = Reg(Bool()) init (False)
  io.answer.valid := ansValid

  ansPayload <> reqData

  io.dCachePort.address := U"32'd0"
  io.dCachePort.isWrite := False
  io.dCachePort.writeValue := B"32'd0"
  io.dCachePort.valid := False
  io.dCachePort.writeMask := B"4'd0"

  val bypassWPort = BypassWritePort().noCombLoopCheck
  val bypassValueReady = Reg(Bool()) init (False)
  io.bypassWritePort := bypassWPort
  bypassWPort.whichReg := U"5'd0"
  bypassWPort.finished := bypassValueReady
  bypassWPort.regValue := B"32'd0"

  io.csrPort.address := 0
  io.csrPort.writeData := 0
  io.csrPort.withWrite := False
  io.csrPort.noRead := False
  io.csrPort.valid := False

  def insertBypass(solvedThisStage: Boolean): Unit = {
    bypassWPort.whichReg := ansPayload.regDest
    bypassWPort.regValue := ansPayload.regDestValue
    bypassValueReady := Bool(solvedThisStage)
  }

  def doLoadStore(requestData: ExecMemPayload): Unit = {
    val addrOffset = requestData.memoryAddress(1 downto 0)
    val dataToWrite = Bits(32 bits)
    dataToWrite := B"32'd0"
    val writeMask = Bits(4 bits)
    writeMask := B"4'd0"

    /* 设置 cache 端口请求 */
    io.dCachePort.address := requestData.memoryAddress
    io.dCachePort.writeValue := dataToWrite
    io.dCachePort.valid := True
    io.dCachePort.writeMask := writeMask

    ansPayload := requestData

    switch(requestData.microOp) {
      is(MicroOp.LOAD) {
        io.dCachePort.isWrite := False
        switch(requestData.function0) {
          is(B"000") {
            /* LB */
            ansPayload.regDestValue := io.dCachePort.readValue.subdivideIn(8 bits)(addrOffset).asSInt.resize(32).asBits
          }
          is(B"001") {
            /* LH */
            ansPayload.regDestValue := io.dCachePort.readValue.subdivideIn(16 bits)(addrOffset(1).asUInt).asSInt.resize(32).asBits
          }
          is(B"010") {
            /* LW */
            ansPayload.regDestValue := io.dCachePort.readValue.asBits
          }
          is(B"100") {
            /* LBU */
            ansPayload.regDestValue := io.dCachePort.readValue.subdivideIn(8 bits)(addrOffset).asUInt.resize(32).asBits
          }
          is(B"101") {
            /* LHU */
            ansPayload.regDestValue := io.dCachePort.readValue.subdivideIn(16 bits)(addrOffset(1).asUInt).asUInt.resize(32).asBits
          }
        }
      }
      is(MicroOp.STORE) {
        io.dCachePort.isWrite := True
        switch(requestData.function0) {
          is(B"000") {
            /* SB */
            dataToWrite := requestData.registerSources(1).value |<< addrOffset.muxList[UInt](
              for (idx <- 0 until 4)
                yield (idx, U(idx * 8, 8 bits))
            )
            writeMask := addrOffset.mux[Bits](
              0 -> B"0001",
              1 -> B"0010",
              2 -> B"0100",
              3 -> B"1000"
            )
          }
          is(B"001") {
            /* SH */
            dataToWrite := requestData.registerSources(1).value |<< addrOffset(1).asUInt.mux[UInt](
              0 -> U"6'd0",
              1 -> U"6'd16"
            )
            writeMask := addrOffset(1).asUInt.mux[Bits](
              0 -> B"0011",
              1 -> B"1100",
            )
          }
          is(B"010") {
            /* SW */
            dataToWrite := requestData.registerSources(1).value
            writeMask := B"1111"
          }
        }
      }
    }
  }

  io.dCacheMiss := False

  io.answer.payload := ansPayload

  val fsm = new StateMachine {
    val normalWorking = new State with EntryPoint
    val cacheMiss = new State

    val fsmReqData = Reg(ExecMemPayload())

    normalWorking.whenIsActive {
      when(ansPayload.writeRegDest) {
        insertBypass(true)
      }

      ansValid := False
      /* TODO: 处理地址越界，产生异常 */
      when(io.request.valid) {
        ansValid := True
        switch(reqData.microOp) {
          is(MicroOp.LOAD, MicroOp.STORE) {
            when(io.dCachePort.needStall) {
              fsmReqData := reqData
              io.dCacheMiss := True
              ansValid := False
              goto(cacheMiss)
            }
            doLoadStore(reqData)
          }
          is(MicroOp.CSR) {
            io.csrPort.valid := True
            io.csrPort.address := reqData.imm(11 downto 0).asUInt
            switch(reqData.function0) {
              is(B"001") {
                /* CSRRW */
                when(reqData.regDest === 0) {
                  io.csrPort.noRead := True
                }
                io.csrPort.withWrite := True
                io.csrPort.writeData := reqData.registerSources(0).value
                ansPayload.regDestValue := io.csrPort.readData
              }
              is(B"010") {
                /* CSRRS */
                ansPayload.regDestValue := io.csrPort.readData
                io.csrPort.writeData := io.csrPort.readData | reqData.registerSources(0).value
                when(reqData.registerSources(0).which =/= 0) {
                  io.csrPort.withWrite := True
                }
              }
              is(B"011") {
                /* CSRRC */
                ansPayload.regDestValue := io.csrPort.readData
                io.csrPort.writeData := io.csrPort.readData & (~reqData.registerSources(0).value)
                when(reqData.registerSources(0).which =/= 0) {
                  io.csrPort.withWrite := True
                }
              }
              is(B"101") {
                /* CSRRWI */
                when(reqData.regDest === 0) {
                  io.csrPort.noRead := True
                }
                io.csrPort.withWrite := True
                io.csrPort.writeData := reqData.function1.resized
                ansPayload.regDestValue := io.csrPort.readData
              }
              is(B"110") {
                /* CSRRSI */
                ansPayload.regDestValue := io.csrPort.readData
                io.csrPort.writeData := io.csrPort.readData | reqData.function1.resize(32)
                when(reqData.function1 =/= 0) {
                  io.csrPort.withWrite := True
                }
              }
              is(B"111") {
                /* CSRRCI */
                ansPayload.regDestValue := io.csrPort.readData
                io.csrPort.writeData := io.csrPort.readData & (~reqData.function1.resize(32))
                when(reqData.function1 =/= 0) {
                  io.csrPort.withWrite := True
                }
              }
            }
          }
        }
      }
    }

    cacheMiss.whenIsActive {
      io.answer.payload := fsmReqData
      ansValid := False
      when(!io.dCachePort.needStall) {
        ansValid := True
        goto(normalWorking)
      }
      doLoadStore(fsmReqData)

      io.dCacheMiss := io.dCachePort.needStall
    }
  }
}
