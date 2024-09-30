package openrv300.pipeline.fifo

import openrv300.pipeline.payload.FetchPayload
import spinal.core._

case class FetchBufferElement() extends Bundle {
    val payload = FetchPayload()
    val valid = Bool()
  }