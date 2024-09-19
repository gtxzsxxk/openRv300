package openrv300

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba4.axi._

object Config {
  def spinal = SpinalConfig(
    targetDirectory = "hw/gen",
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = LOW
    ),
    onlyStdLogicVectorAtTopLevelIo = true
  )

  def sim = SimConfig.withConfig(spinal).withVcdWave

  def riscvToolchain = "riscv64-unknown-elf"
  def arch = "rv32i"
  def abi = "ilp32"

  def axiConfig = Axi4Config(
      addressWidth = 32,
      dataWidth = 32,
      idWidth = 4,
      useRegion = false,
      useLock = false,
      useCache = false,
      useQos = false,
      useProt = false,

}
