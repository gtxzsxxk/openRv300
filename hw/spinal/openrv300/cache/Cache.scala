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

  val cacheMemories = Mem(Bits(cacheGroupBits bits), wordCount = 64)

  def getCacheLine(way: UInt, index: UInt): CacheLine = cacheMemories(index.resized).subdivideIn(ways slices)(way.resized).as(CacheLine())

  val tag = io.corePort.address(31 downto 12)
  val index = io.corePort.address(11 downto 6)
  val offset = io.corePort.address(5 downto 0)
  val offsetByWord = io.corePort.address(5 downto 2)

  val cacheLine = CacheLine().noCombLoopCheck
  cacheLine.valid := False
  cacheLine.dirty := False
  cacheLine.tag := 0
  cacheLine.counter := 0
  for (idx <- 0 until 16) {
    cacheLine.data(idx) := 0
  }

  val cacheHitVec = Vec.fill(ways)(Bool())
  val cacheLineValids = Vec.fill(ways)(Bool())

  val hit = cacheHitVec.orR
  val whichWay = UInt(log2Up(ways) bits)
  whichWay := 0
  val (hasFreeLine, freeLineWay): (Bool, UInt) = cacheLineValids.sFindFirst(_ === False)

  for (way <- 0 until ways) {
    cacheHitVec(way) := False
    cacheLineValids(way) := False
    when(getCacheLine(way, index).tag === tag && getCacheLine(way, index).valid) {
      cacheLine := getCacheLine(way, index)
      cacheHitVec(way) := True
      whichWay := way
      cacheLineValids(way) := getCacheLine(way, index).valid
    }
  }

  io.corePort.needStall := False
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
    val fsmTag = Reg(UInt(20 bits))
    val fsmIndex = Reg(UInt(6 bits))
    val fsmOffset = Reg(UInt(6 bits))
    val fsmOffsetByWord = fsmOffset(5 downto 2)
    val fsmNeedStall = Reg(Bool())

    val fsmTempline = getCacheLine(whichWayToEvict, fsmIndex).noCombLoopCheck

    val cacheNormalWorking = new State with EntryPoint
    val findWayToEvict = new State
    val doEvict = new State
    val writeDirty = new State
    val writeWaitB = new State
    val readCacheLine = new State
    val finish = new State

    cacheNormalWorking.whenIsActive {
      when(io.corePort.valid) {
        evictCounter := 0
        whichWayToEvictCnt := U"64'hFFFF_FFFF_FFFF_FFFF"
        writeDirtyFlag := False
        when(hit) {
          val group = Bits(cacheGroupBits bits)
          group := cacheMemories(index)
          group.subdivideIn(ways slices)(whichWay) := cacheLine.asBits
          when(io.corePort.isWrite) {
            cacheLine.data(offsetByWord) := io.corePort.writeValue
            cacheLine.dirty := True
          } otherwise {
            io.corePort.readValue := cacheLine.data(offsetByWord)
          }
          cacheLine.counter := cacheLine.counter + 1
          cacheMemories.write(index, group)
          io.corePort.needStall := False
        } otherwise {
          /* cache miss */
          fsmTag := tag
          fsmIndex := index
          fsmOffset := offset
          fsmIsWrite := io.corePort.isWrite

          when(io.corePort.isWrite) {
            fsmWriteValue := io.corePort.writeValue
          }

          when(hasFreeLine) {
            whichWayToEvict := freeLineWay
            goto(doEvict)
          } otherwise {
            if (ways <= 2) {
              whichWayToEvict := 0
              goto(doEvict)
            } else {
              goto(findWayToEvict)
            }
          }

          /* 立刻stall */
          io.corePort.needStall := True
        }
      }
    }


    findWayToEvict.onEntry(fsmNeedStall := True).whenIsActive {
      val line1 = getCacheLine(evictCounter, fsmIndex).counter
      val line2 = getCacheLine(evictCounter + 1, fsmIndex).counter
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

      when(evictCounter + 1 === U(ways)) {
        goto(doEvict)
      } otherwise {
        evictCounter := evictCounter + 2
      }

      io.corePort.needStall := fsmNeedStall
    }


    doEvict.onEntry(fsmNeedStall := True).whenIsActive {
      val toEvictLine = getCacheLine(whichWayToEvict, fsmIndex)
      when(toEvictLine.valid && toEvictLine.dirty) {
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
      val toEvictLine = getCacheLine(whichWayToEvict, fsmIndex)
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
          b.setBlocked()
          goto(readCacheLine)
        }
      }

      io.corePort.needStall := fsmNeedStall
    }

    readCacheLine.onEntry(fsmNeedStall := True).onEntry(readStartFlag := False).onEntry(readCnt := 0).whenIsActive {
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
            fsmTempline.valid := True
            fsmTempline.dirty := False
            fsmTempline.tag := fsmTag
            fsmTempline.counter := 0
            fsmTempline.data(readCnt.resized) := r.data

            val group = Bits(cacheGroupBits bits)
            group := cacheMemories(index)
            group.subdivideIn(ways slices)(whichWayToEvict) := fsmTempline.asBits
            cacheMemories.write(index, group)

            readCnt := readCnt + 1
          }
          when(readCnt === 16 - 1) {
            goto(finish)
          }
        }
      }

      io.corePort.needStall := fsmNeedStall
    }

    finish.onEntry(fsmNeedStall := False).whenIsActive {
      io.memPort.r.setBlocked()

      val group = Bits(cacheGroupBits bits)
      group := cacheMemories(fsmIndex)
      group.subdivideIn(ways slices)(whichWayToEvict) := fsmTempline.asBits

      when(fsmIsWrite) {
        fsmTempline.data(fsmOffsetByWord) := fsmWriteValue
        fsmTempline.dirty := True
      } otherwise {
        io.corePort.readValue := fsmTempline.data(fsmOffsetByWord)
      }

      fsmTempline.counter := fsmTempline.counter + 1
      cacheMemories.write(fsmIndex, group)
      goto(cacheNormalWorking)

      io.corePort.needStall := False
    }
  }
}
