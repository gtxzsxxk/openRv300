package openrv300.pipeline

import openrv300.isa.MicroOp
import openrv300.pipeline.payload._
import spinal.core._
import spinal.lib._

case class MemAccess() extends Component {
  val io = new Bundle {
    val request = slave(Flow(ExecMemPayload()))
    val answer = master(Flow(ExecMemPayload()))
  }

  val dataMem = Mem(Bits(32 bits), wordCount = 256)

  val reqData = io.request.payload
  val ansPayload = Reg(ExecMemPayload())

  ansPayload <> reqData

  val addrByWord = UInt(30 bits)
  addrByWord := reqData.memoryAddress(31 downto 2)
  val addrOffset = UInt(2 bits)
  addrOffset := reqData.memoryAddress(1 downto 0)
  val dataToWrite = Bits(32 bits)
  val writeMask = Bits(4 bits)

  io.answer.setIdle()

  /* TODO: 处理地址越界，产生异常 */

  when(io.request.valid) {
    io.answer.push(ansPayload)

    switch(reqData.microOp) {
      is(MicroOp.LOAD) {
        switch(reqData.function0) {
          is(B"000") {
            /* LB */
            ansPayload.regDestValue := dataMem(addrByWord).subdivideIn(8 bits)(addrOffset).asSInt.resize(32)
          }
          is(B"001") {
            /* LH */
            ansPayload.regDestValue := dataMem(addrByWord).subdivideIn(16 bits)(addrOffset(1).asUInt).asSInt.resize(32)
          }
          is(B"010") {
            /* LW */
            ansPayload.regDestValue := dataMem(addrByWord)
          }
          is(B"100") {
            /* LBU */
            ansPayload.regDestValue := dataMem(addrByWord).subdivideIn(8 bits)(addrOffset).asUInt.resize(32)
          }
          is(B"101") {
            /* LHU */
            ansPayload.regDestValue := dataMem(addrByWord).subdivideIn(16 bits)(addrOffset(1).asUInt).asUInt.resize(32)
          }
        }
      }
      is(MicroOp.STORE) {
        is(B"000") {
          /* SB */
          dataToWrite := reqData.regSource1.value |<< addrOffset.muxList[UInt](
            for (idx <- 0 until 4)
              yield (idx, idx * 8)
          )
          writeMask := addrOffset.mux[Bits](
            0 -> B"0001",
            1 -> B"0010",
            2 -> B"0100",
            3 -> B"1000"
          )
          dataMem.write(addrByWord, dataToWrite, mask = writeMask)
        }
        is(B"001") {
          /* SH */
          dataToWrite := reqData.regSource1.value |<< addrOffset(1).mux[UInt](
            0 -> U"6'd0",
            1 -> U"6'd16"
          )
          writeMask := addrOffset(1).mux[Bits](
            0 -> B"01",
            1 -> B"10",
          )
          dataMem.write(addrByWord, dataToWrite, mask = writeMask)
        }
        is(B"010") {
          /* SW */
          dataMem.write(addrByWord, reqData.regSource1.value)
        }
      }
    }
  }
}
