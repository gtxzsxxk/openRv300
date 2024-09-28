package openrv300

import openrv300.OpenRv300
import openrv300.ddr._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

case class OpenRv300SimTop() extends Component {
  val core = OpenRv300()
  val ddr = DDRSim()
  val axiCrossbar = Axi4CrossbarFactory()
  axiCrossbar.addSlaves(
    ddr.io.memPort -> (0x80000000L, 128 MiB)
  )
  axiCrossbar.addConnections(
    core.io.bus -> List(ddr.io.memPort)
  )

  axiCrossbar.build()
}
