package openrv300.privilege

import spinal.core._
import spinal.lib._

object PrivilegeLevels extends SpinalEnum {
  val user, supervisor, machine = newElement()
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    user -> 0,
    supervisor -> 1,
    machine -> 3,
  )
}
