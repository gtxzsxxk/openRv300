package openrv300.cache

import openrv300.Config.axiConfig
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm._

case class Cache(ways: Int) extends Component {
  val io = new Bundle {
    val corePort = slave(CacheCorePort())
    val memPort = master(Axi4(axiConfig))
  }

  def cacheLineSize: Int = widthOf(CacheLine())

  def cacheGroupBits: Int = ways * cacheLineSize

  def cacheGroupFlagBits: Int = widthOf(CacheLineFlags(ways))

  val cacheMemories = Mem(Bits(cacheGroupBits bits), wordCount = 64)
  val cacheFlags = Reg(Vec.fill(64)(Bits(cacheGroupFlagBits bits)))

  for(idx <- 0 until 64) {
    cacheFlags(idx) init (B(0, cacheGroupFlagBits bits))
  }

  def getCacheLine(way: UInt, index: UInt): (CacheLine, CacheLineFlags) = {
    val line = cacheMemories(index.resized).subdivideIn(ways slices)(way.resized).as(CacheLine())
    val flags = cacheFlags(index.resized).as(CacheLineFlags(ways))
    (line, flags)
  }

  val tag = io.corePort.address(31 downto 12)
  val index = io.corePort.address(11 downto 6)
  val offset = io.corePort.address(5 downto 0)
  val offsetByWord = io.corePort.address(5 downto 2)

  val cacheLine = CacheLine().noCombLoopCheck
  cacheLine.tag := 0
  for (idx <- 0 until 16) {
    cacheLine.data(idx) := 0
  }

  val cacheLineFlags = CacheLineFlags(ways)
  cacheLineFlags := cacheFlags(index).as(CacheLineFlags(ways))

  val cacheHitVec = Vec.fill(ways)(Bool())
  val cacheLineValids = Vec.fill(ways)(Bool())

  val hit = cacheHitVec.orR
  val whichWay = UInt(log2Up(ways) bits)
  whichWay := 0
  val (hasFreeLine, freeLineWay): (Bool, UInt) = cacheLineValids.sFindFirst(_ === False)

  for (way <- 0 until ways) {
    cacheHitVec(way) := False
    cacheLineValids(way) := getCacheLine(way, index)._2.validVec(way)
    when(getCacheLine(way, index)._1.tag === tag && getCacheLine(way, index)._2.validVec(way)) {
      cacheLine := getCacheLine(way, index)._1
      cacheHitVec(way) := True
      whichWay := way
    }
  }

  io.corePort.needStall := True
  io.corePort.readValue := 0
  io.memPort.aw.setIdle()
  io.memPort.ar.setIdle()
  io.memPort.w.setIdle()
  io.memPort.r.setBlocked()
  io.memPort.b.setBlocked()

  val fsm = new StateMachine {
    val evictCounter = Reg(UInt(4 bits))
    val whichWayToEvict = Reg(UInt(log2Up(ways) bits))
    val whichWayToEvictCnt = Reg(UInt(64 bits))
    val writeDirtyFlag = Reg(Bool())
    val writeDirtyCnt = Reg(UInt(6 bits))
    val readStartFlag = Reg(Bool())
    val readCnt = Reg(UInt(6 bits))

    val fsmIsWrite = Reg(Bool())
    val fsmWriteValue = Reg(Bits(32 bits))
    val fsmWriteMask = Reg(Bits(4 bits))
    val fsmTag = Reg(UInt(20 bits))
    val fsmIndex = Reg(UInt(6 bits))
    val fsmOffset = Reg(UInt(6 bits))
    val fsmOffsetByWord = fsmOffset(5 downto 2)
    val fsmNeedStall = Reg(Bool()) init (False)

    val fsmTempLine = CacheLine().noCombLoopCheck
    fsmTempLine := getCacheLine(whichWayToEvict, fsmIndex)._1
    val fsmTempLineFlags = CacheLineFlags(ways)
    fsmTempLineFlags := getCacheLine(whichWayToEvict, fsmIndex)._2

    val cacheNormalWorking = new State with EntryPoint
    val findWayToEvict = new State
    val doEvict = new State
    val writeDirty = new State
    val writeWaitB = new State
    val readCacheLine = new State

    cacheNormalWorking.whenIsActive {
      io.memPort.r.setBlocked()

      when(io.corePort.valid) {
        evictCounter := 0
        whichWayToEvictCnt := U"64'hFFFF_FFFF_FFFF_FFFF"
        writeDirtyFlag := False
        when(hit) {
          val group = Bits(cacheGroupBits bits)
          group := cacheMemories(index)
          group.subdivideIn(ways slices)(whichWay) := cacheLine.asBits
          when(io.corePort.isWrite) {
            for (idx <- 0 until 4) {
              when(io.corePort.writeMask(idx) === True) {
                cacheLine.data(offsetByWord).subdivideIn(4 slices)(idx) :=
                  io.corePort.writeValue.subdivideIn(4 slices)(idx)
              }
            }
            cacheLineFlags.dirtyVec(whichWay) := True
          } otherwise {
            io.corePort.readValue := cacheLine.data(offsetByWord)
          }

          val writeTmpCacheLineFlags = CacheLineFlags(ways)
          writeTmpCacheLineFlags := cacheLineFlags
          writeTmpCacheLineFlags.counterVec(whichWay) := cacheLineFlags.counterVec(whichWay) + 1
          cacheMemories.write(index, group)
          cacheFlags(index) := writeTmpCacheLineFlags.asBits
          io.corePort.needStall := False
        } otherwise {
          /* cache miss */
          fsmTag := tag
          fsmIndex := index
          fsmOffset := offset
          fsmIsWrite := io.corePort.isWrite

          when(io.corePort.isWrite) {
            fsmWriteValue := io.corePort.writeValue
            fsmWriteMask := io.corePort.writeMask
          }

          when(hasFreeLine) {
            whichWayToEvict := freeLineWay
            goto(doEvict)
          } otherwise {
            goto(findWayToEvict)
          }

          /* 立刻stall */
          io.corePort.needStall := True
        }
      }
    }


    findWayToEvict.onEntry(fsmNeedStall := True).whenIsActive {
      val line1 = getCacheLine(evictCounter, fsmIndex)._2.counterVec(evictCounter.resized)
      val line2 = getCacheLine(evictCounter + 1, fsmIndex)._2.counterVec((evictCounter + 1).resized)
      when(line1 < line2) {
        when(line1 < whichWayToEvictCnt) {
          whichWayToEvictCnt := line1
          whichWayToEvict := evictCounter.resized
        }
      } otherwise {
        when(line2 < whichWayToEvictCnt) {
          whichWayToEvictCnt := line2
          whichWayToEvict := (evictCounter + 1).resized
        }
      }

      when(evictCounter + 2 === U(ways)) {
        goto(doEvict)
      } otherwise {
        evictCounter := evictCounter + 2
      }

      io.corePort.needStall := fsmNeedStall
    }


    doEvict.onEntry(fsmNeedStall := True).whenIsActive {
      val toEvictLine = getCacheLine(whichWayToEvict, fsmIndex)._1
      val toEvictLineFlags = getCacheLine(whichWayToEvict, fsmIndex)._2
      when(toEvictLineFlags.validVec(whichWayToEvict) && toEvictLineFlags.dirtyVec(whichWayToEvict)) {
        val aw = io.memPort.aw
        aw.valid := True
        aw.payload.id := 1
        aw.payload.addr := Cat(toEvictLine.tag, fsmIndex, U"6'd0").asUInt
        /* 传输64个字节 */
        aw.payload.len := 64 - 1
        /* 一次传4个字节 */
        aw.payload.size := Axi4.size.BYTE_4.asUInt
        /* 增量burst */
        aw.payload.burst := Axi4.burst.INCR

        when(aw.ready) {
          goto(writeDirty)
        }
      } otherwise {
        goto(readCacheLine)
      }

      io.corePort.needStall := fsmNeedStall
    }

    writeDirty.onEntry(fsmNeedStall := True).onEntry(writeDirtyCnt := 0).whenIsActive {
      io.memPort.aw.setIdle()
      val w = io.memPort.w
      val toEvictLine = getCacheLine(whichWayToEvict, fsmIndex)._1
      when(writeDirtyCnt <= 16 - 1) {
        w.data := toEvictLine.data(writeDirtyCnt.resized)
        w.strb := B"4'b1111"
        w.last := writeDirtyCnt === 16 - 1
        w.valid := True

        when(w.ready) {
          writeDirtyCnt := writeDirtyCnt + 1
        }
      } otherwise {
        w.setIdle()
        goto(writeWaitB)
      }

      io.corePort.needStall := fsmNeedStall
    }

    writeWaitB.onEntry(fsmNeedStall := True).whenIsActive {
      val b = io.memPort.b
      b.ready := True
      when(b.valid) {
        /* TODO: 处理异常 */
        when(b.resp === Axi4.resp.OKAY) {
          goto(readCacheLine)
        }
      }

      io.corePort.needStall := fsmNeedStall
    }

    readCacheLine.onEntry(fsmNeedStall := True).onEntry(readStartFlag := False).onEntry(readCnt := 0).whenIsActive {
      io.memPort.b.setBlocked()

      val ar = io.memPort.ar
      val r = io.memPort.r
      ar.valid := True
      ar.payload.id := 0
      ar.payload.addr := Cat(fsmTag, fsmIndex, U"6'd0").asUInt
      /* 传输64个字节 */
      ar.payload.len := 64 - 1
      /* 一次传4个字节 */
      ar.payload.size := Axi4.size.BYTE_4.asUInt
      /* 增量burst */
      ar.payload.burst := Axi4.burst.INCR

      when(ar.ready) {
        readStartFlag := True
      }

      when(readStartFlag) {
        ar.setIdle()
        r.ready := True
        when(r.valid) {
          /* TODO: 处理异常 */
          when(r.resp === Axi4.resp.OKAY) {
            fsmTempLine.tag := fsmTag
            fsmTempLine.data(readCnt.resized) := r.data

            val group = Bits(cacheGroupBits bits)
            group := cacheMemories(fsmIndex)
            group.subdivideIn(ways slices)(whichWayToEvict) := fsmTempLine.asBits
            cacheMemories.write(fsmIndex, group)

            readCnt := readCnt + 1
          }
          when(readCnt === 16 - 1) {
            fsmTempLineFlags.validVec(whichWayToEvict) := True
            fsmTempLineFlags.dirtyVec(whichWayToEvict) := False
            fsmTempLineFlags.counterVec(whichWayToEvict) := U"64'd0"
            cacheFlags(fsmIndex) := fsmTempLineFlags.asBits
            fsmNeedStall := False
            goto(cacheNormalWorking)
          }
        }
      }

      io.corePort.needStall := fsmNeedStall
    }
  }
}
