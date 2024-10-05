package openrv300.isa

object ExceptionCode {
  def InstAddrMisaligned = 0
  def InstAccessFault = 1
  def IllegalInstruction = 2
  def Breakpoint = 3
  def LoadAddrMisaligned = 4
  def LoadAccessFault = 5
  def StoreAMOAddrMisaligned = 6
  def StoreAMOAccessFault = 7
  def EcallFromUser = 8
  def EcallFromSupervisor = 9
  def EcallFromMachine = 11
  def InstPageFault = 12
  def LoadPageFault = 13
  def StoreAMOPageFault = 15
}
