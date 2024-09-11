package openrv300

import spinal.core._
import spinal.lib.master
import spinal.lib.bus.amba4.axi._

case class OpenRv300() extends Component {
  val io = new Bundle {
    val bus = master(Axi4(Axi4Config(
      addressWidth = 32,
      dataWidth = 32,
      idWidth = 4,
      useRegion = false,
      useLock = false,
      useCache = false,
      useQos = false,
      useProt = false,
    )))
  }

  

}
