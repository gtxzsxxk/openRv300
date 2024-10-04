package openrv300.privilege

import spinal.core._
import spinal.lib._

case class CSRs() extends Component {
  object CSRReadWrite extends SpinalEnum {
    val readOnly, readWrite = newElement()
  }

  val privilegeLevel = Reg(Bits(2 bits)) init (PrivilegeLevels.machine.asBits)

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
}
