package openrv300.pipeline

import openrv300.cache.CacheCorePort
import openrv300.isa.MicroOp
import openrv300.pipeline.control.BypassWritePort
import openrv300.pipeline.payload._
import spinal.core._
import spinal.lib._

case class MemAccess() extends Component {
  val io = new Bundle {
    val request = slave(Flow(ExecMemPayload()))
    /* answer.valid 决定了是否应该停止流水线 */
    val answer = master(Flow(ExecMemPayload()))
    val bypassWritePort = master(BypassWritePort())
    val dCachePort = master(CacheCorePort())
  }

  val dataMem = Mem(Bits(32 bits), wordCount = 256)

  val reqData = io.request.payload
  val ansPayload = Reg(ExecMemPayload())

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
            dataMem.write(addrByWord, reqData.registerSources(1).value)
          }
        }
            dataToWrite := requestData.registerSources(1).value
          }
        }
      }
    }
  }
      }
    }
  }
}
