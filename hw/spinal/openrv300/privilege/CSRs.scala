package openrv300.privilege

import spinal.core._
import spinal.lib._

case class CSRs() extends Component {
  val io = new Bundle {
    val port = slave(CSRPort())
    /* 与 write back 连在一起 */
    val throwTrapPort = slave(ThrowTrapRequest())
    val doTrapPort = master(DoTrapRequest())

    /* 如果 trap 来自没有执行的分支的指令，就需要清楚trap */
    val clearTrap = in port Bool()

    /* 组合逻辑，产生 Trap，需要清空暂未提交的指令 */
    /* 分别对应译码阶段、执行、访存阶段需要 stall，
     * 如果取指出了问题，不会影响其它流水段 */
    val throwTrapNow = in port Bits(3 bits)
    /* 分别对应取指阶段、译码阶段、执行、访存阶段需要 stall */
    val csrNeedStall = out port Bits(4 bits)

    val privilegeLevel = out port Bits(2 bits)
  }

  io.port.readData := 0

  object CSRReadWrite extends SpinalEnum {
    val readOnly, readWrite = newElement()
  }

  val privilegeLevel = Reg(Bits(2 bits)) init (PrivilegeLevels.machine.asBits)
  io.privilegeLevel := privilegeLevel

  val csrListings = Seq(
    U"12'h100" -> (PrivilegeLevels.supervisor, CSRReadWrite.readWrite) -> "sstatus",
    U"12'h104" -> (PrivilegeLevels.supervisor, CSRReadWrite.readWrite) -> "sie",
    U"12'h105" -> (PrivilegeLevels.supervisor, CSRReadWrite.readWrite) -> "stvec",
    U"12'h106" -> (PrivilegeLevels.supervisor, CSRReadWrite.readWrite) -> "scounteren",
    U"12'h10A" -> (PrivilegeLevels.supervisor, CSRReadWrite.readWrite) -> "senvcfg",
    U"12'h140" -> (PrivilegeLevels.supervisor, CSRReadWrite.readWrite) -> "sscratch",
    U"12'h141" -> (PrivilegeLevels.supervisor, CSRReadWrite.readWrite) -> "sepc",
    U"12'h142" -> (PrivilegeLevels.supervisor, CSRReadWrite.readWrite) -> "scause",
    U"12'h143" -> (PrivilegeLevels.supervisor, CSRReadWrite.readWrite) -> "stval",
    U"12'h144" -> (PrivilegeLevels.supervisor, CSRReadWrite.readWrite) -> "sip",
    U"12'h14D" -> (PrivilegeLevels.supervisor, CSRReadWrite.readWrite) -> "stimecmp",
    U"12'h15D" -> (PrivilegeLevels.supervisor, CSRReadWrite.readWrite) -> "stimecmph",
    U"12'h180" -> (PrivilegeLevels.supervisor, CSRReadWrite.readWrite) -> "satp",
    U"12'h5A8" -> (PrivilegeLevels.supervisor, CSRReadWrite.readWrite) -> "scontext",

    U"12'h300" -> (PrivilegeLevels.machine, CSRReadWrite.readWrite) -> "mstatus",
    U"12'h301" -> (PrivilegeLevels.machine, CSRReadWrite.readWrite) -> "misa",
    U"12'h302" -> (PrivilegeLevels.machine, CSRReadWrite.readWrite) -> "medeleg",
    U"12'h303" -> (PrivilegeLevels.machine, CSRReadWrite.readWrite) -> "mideleg",
    U"12'h304" -> (PrivilegeLevels.machine, CSRReadWrite.readWrite) -> "mie",
    U"12'h305" -> (PrivilegeLevels.machine, CSRReadWrite.readWrite) -> "mtvec",
    U"12'h306" -> (PrivilegeLevels.machine, CSRReadWrite.readWrite) -> "mcounteren",
    U"12'h30A" -> (PrivilegeLevels.machine, CSRReadWrite.readWrite) -> "menvcfg",
    U"12'h310" -> (PrivilegeLevels.machine, CSRReadWrite.readWrite) -> "mstatush",
    U"12'h31A" -> (PrivilegeLevels.machine, CSRReadWrite.readWrite) -> "menvcfgh",
    U"12'h320" -> (PrivilegeLevels.machine, CSRReadWrite.readWrite) -> "mcountinhibit",
    U"12'h340" -> (PrivilegeLevels.machine, CSRReadWrite.readWrite) -> "mscratch",
    U"12'h341" -> (PrivilegeLevels.machine, CSRReadWrite.readWrite) -> "mepc",
    U"12'h342" -> (PrivilegeLevels.machine, CSRReadWrite.readWrite) -> "mcause",
    U"12'h343" -> (PrivilegeLevels.machine, CSRReadWrite.readWrite) -> "mtval",
    U"12'h344" -> (PrivilegeLevels.machine, CSRReadWrite.readWrite) -> "mip",
    U"12'h34A" -> (PrivilegeLevels.machine, CSRReadWrite.readWrite) -> "mtinst",
    U"12'hF11" -> (PrivilegeLevels.machine, CSRReadWrite.readOnly) -> "mvendorid",
    U"12'hF12" -> (PrivilegeLevels.machine, CSRReadWrite.readOnly) -> "marchid",
    U"12'hF13" -> (PrivilegeLevels.machine, CSRReadWrite.readOnly) -> "mimpid",
    U"12'hF14" -> (PrivilegeLevels.machine, CSRReadWrite.readOnly) -> "mhartid",
    U"12'hF15" -> (PrivilegeLevels.machine, CSRReadWrite.readOnly) -> "mconfigptr",


    U"12'hC00" -> (PrivilegeLevels.user, CSRReadWrite.readOnly) -> "cycle",
    U"12'hC01" -> (PrivilegeLevels.user, CSRReadWrite.readOnly) -> "time",
    U"12'hC80" -> (PrivilegeLevels.user, CSRReadWrite.readOnly) -> "cycleh",
    U"12'hC81" -> (PrivilegeLevels.user, CSRReadWrite.readOnly) -> "timeh",
  )

  def getCsrIndexByName(name: String): Int = {
    var index = -1
    for (idx <- csrListings.indices) {
      if (csrListings(idx)._2 == name) {
        index = idx
      }
    }
    index
  }

  val csrEntities = Vec.fill(csrListings.size)(Reg(Bits(32 bits)))

  /* 复位后的初值 */
  csrEntities(getCsrIndexByName("mvendorid")) init (B"32'h0")
  csrEntities(getCsrIndexByName("mhartid")) init (B"32'h0")
  csrEntities(getCsrIndexByName("medeleg")) init (B"32'h0")
  csrEntities(getCsrIndexByName("cycle")) init (B"32'h0")
  csrEntities(getCsrIndexByName("cycleh")) init (B"32'h0")
  csrEntities(getCsrIndexByName("time")) init (B"32'h0")
  csrEntities(getCsrIndexByName("timeh")) init (B"32'h0")
  csrEntities(getCsrIndexByName("stimecmp")) init (B"32'h0")
  csrEntities(getCsrIndexByName("stimecmph")) init (B"32'h0")

  when(io.port.valid) {
    switch(io.port.address) {
      for (idx <- csrListings.indices) {
        is(csrListings(idx)._1._1) {
          when(csrListings(idx)._1._2._1.asBits.asUInt > privilegeLevel.asUInt) {
            /* TODO: 权限不够，产生异常 */
          } otherwise {
            when(io.port.withWrite && csrListings(idx)._1._2._2 === CSRReadWrite.readOnly) {
              /* TODO: 只能读，产生异常 */
            } otherwise {
              when(io.port.withWrite) {
                csrEntities(idx) := io.port.writeData
                /* TODO: 写的副作用 */
              }
              when(!io.port.noRead) {
                /* TODO: 读的副作用，但是标准的 CSR 没有读的副作用 */
                io.port.readData := csrEntities(idx)
              }
            }
          }
        }
      }
      default {
        /* TODO: exception */
      }
    }
  }

  /* 进入异常 */
  val trapJumpAddress = Reg(UInt(32 bits))
  val trapValid = Reg(Bool()) init (False)

  io.doTrapPort.trapJumpAddress := trapJumpAddress
  io.doTrapPort.trapValid := trapValid

  val trapInfo = io.throwTrapPort
  when(trapInfo.throwTrap) {
    when(csrEntities(getCsrIndexByName("medeleg"))(trapInfo.trapCause.resized)) {
      /* delegate to supervisor */
      csrEntities(getCsrIndexByName("scause")) := trapInfo.trapCause.asBits.resized & B"32'h7fff_ffff"
      csrEntities(getCsrIndexByName("sepc")) := trapInfo.trapPc.asBits
      csrEntities(getCsrIndexByName("stval")) := trapInfo.trapValue

      /* SPP */
      csrEntities(getCsrIndexByName("sstatus"))(8) := privilegeLevel(0)
      /* SPIE */
      csrEntities(getCsrIndexByName("sstatus"))(5) := csrEntities(getCsrIndexByName("sstatus"))(1) /* SIE */
      /* Set SIE to Zero */
      csrEntities(getCsrIndexByName("sstatus"))(1) := False

      /* 给出应当跳转到的 Handler 地址 */
      trapValid := True
      when(csrEntities(getCsrIndexByName("stvec"))(1 downto 0).asUInt === 0) {
        /* All traps set pc to BASE */
        trapJumpAddress := Cat(csrEntities(getCsrIndexByName("stvec"))(31 downto 2), B"00").asUInt
      } elsewhen (csrEntities(getCsrIndexByName("stvec"))(1 downto 0).asUInt === 1) {
        trapJumpAddress := Cat(csrEntities(getCsrIndexByName("stvec"))(31 downto 2), B"00").asUInt +
          (trapInfo.trapCause.asBits.resize(32) |<< 2).asUInt
      }

      /* 进入 Supervisor */
      privilegeLevel := PrivilegeLevels.supervisor.asBits
    } otherwise {
      csrEntities(getCsrIndexByName("mcause")) := trapInfo.trapCause.asBits.resized & B"32'h7fff_ffff"
      csrEntities(getCsrIndexByName("mepc")) := trapInfo.trapPc.asBits
      csrEntities(getCsrIndexByName("mtval")) := trapInfo.trapValue

      /* MPP */
      csrEntities(getCsrIndexByName("mstatus"))(12 downto 11) := privilegeLevel
      /* MPIE */
      csrEntities(getCsrIndexByName("mstatus"))(7) := csrEntities(getCsrIndexByName("mstatus"))(3) /* MIE */
      /* Set MIE to Zero */
      csrEntities(getCsrIndexByName("mstatus"))(3) := False

      /* 给出应当跳转到的 Handler 地址 */
      trapValid := True
      when(csrEntities(getCsrIndexByName("mtvec"))(1 downto 0).asUInt === 0) {
        /* All traps set pc to BASE */
        trapJumpAddress := Cat(csrEntities(getCsrIndexByName("mtvec"))(31 downto 2), B"00").asUInt
      } elsewhen (csrEntities(getCsrIndexByName("mtvec"))(1 downto 0).asUInt === 1) {
        trapJumpAddress := Cat(csrEntities(getCsrIndexByName("mtvec"))(31 downto 2), B"00").asUInt +
          (trapInfo.trapCause.asBits.resize(32) |<< 2).asUInt
      }

      /* 进入 Machine 模式 */
      privilegeLevel := PrivilegeLevels.machine.asBits
    }
  }

  val csrNeedStallReg = Reg(Bits(4 bits)) init (0)

  io.csrNeedStall := csrNeedStallReg

  when(io.clearTrap) {
    csrNeedStallReg := 0
    trapValid := False
  } otherwise {
    switch(io.throwTrapNow) {
      is(B"001") {
        /* 译码 */
        io.csrNeedStall := B"0011"
        csrNeedStallReg := io.csrNeedStall
      }
      is(M"01-") {
        /* 执行 */
        io.csrNeedStall := B"0111"
        csrNeedStallReg := io.csrNeedStall
      }
      is(M"1--") {
        /* MEM */
        io.csrNeedStall := B"1111"
        csrNeedStallReg := io.csrNeedStall
      }
    }
  }

  when(io.doTrapPort.trapReady) {
    //    io.csrNeedStall := 0
    csrNeedStallReg := 0
    trapValid := False
  }
}
