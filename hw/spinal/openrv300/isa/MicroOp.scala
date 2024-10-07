package openrv300.isa

import spinal.core._

object MicroOp {
  def LUI = B"7'd1"
  def AUIPC = B"7'd2"
  def JAL = B"7'd3"
  def JALR = B"7'd4"
  def BRANCH = B"7'd5"
  def LOAD = B"7'd6"
  def STORE = B"7'd7"
  def ARITH_BINARY_IMM = B"7'd8"
  def ARITH_SLL_IMM = B"7'd13"
  def ARITH_SRL_IMM = B"7'd14"
  def ARITH_SRA_IMM = B"7'd15"
  def ARITH_BINARY = B"7'd10"
  def ARITH_SLL = B"7'd16"
  def ARITH_SRL = B"7'd17"
  def ARITH_SRA = B"7'd18"
  def FENCE = B"7'd12"
  def FENCE_TSO = B"7'd21"
  def ECALL = B"7'd19"
  def CSR = B"7'd20"
}
