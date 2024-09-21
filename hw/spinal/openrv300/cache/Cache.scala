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

  def getCacheLine(way: UInt, index: UInt): CacheLine = cacheMemories(index).subdivideIn(2 slices)(way).as(CacheLine())

  val tag = io.corePort.address(31 downto 12)
  val index = io.corePort.address(11 downto 6)
  val offset = io.corePort.address(5 downto 0)
  val offsetByWord = io.corePort.address(5 downto 2)

  val cacheLine = CacheLine()

  val cacheHitVec = Vec.fill(ways)(Bool())
  val cacheLineValids = Vec.fill(ways)(Bool())
  cacheLine.valid := False

  val (hit, whichWay): (Bool, UInt) = cacheHitVec.sFindFirst(_ === True)
  val (hasFreeLine, freeLineWay): (Bool, UInt) = cacheLineValids.sFindFirst(_ === False)

  for (way <- 0 until ways) {
    cacheHitVec(way) := False
    when(getCacheLine(way, index).tag === tag && getCacheLine(way, index).valid) {
      cacheLine := getCacheLine(way, index)
      cacheHitVec(way) := True
    }
    cacheLineValids(way) := getCacheLine(way, index).valid
  }

  val fsm = new StateMachine {
    val evictCounter = Reg(UInt(log2Up(ways) bits))
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
          val group = cacheMemories(index)
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


    findWayToEvict.onEntry(fsmNeedStall := True).onEntry(io.corePort.needStall := fsmNeedStall).whenIsActive {
      val line1 = getCacheLine(evictCounter, fsmIndex).counter
      val line2 = getCacheLine(evictCounter + 1, fsmIndex).counter
      when(line1 < line2) {
        when(line1 < whichWayToEvictCnt) {
          whichWayToEvictCnt := line1
          whichWayToEvict := evictCounter
        }
      } otherwise {
        when(line2 < whichWayToEvictCnt) {
          whichWayToEvictCnt := line2
          whichWayToEvict := evictCounter + 1
        }
      }

      when(evictCounter + 1 === U(ways)) {
        goto(doEvict)
      } otherwise {
        evictCounter := evictCounter + 2
      }
    }


    doEvict.onEntry(fsmNeedStall := True).onEntry(io.corePort.needStall := fsmNeedStall).whenIsActive {
      val toEvictLine = getCacheLine(whichWayToEvict, fsmIndex)
      when(toEvictLine.dirty) {
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
    }

    writeDirty.onEntry(fsmNeedStall := True).onEntry(io.corePort.needStall := fsmNeedStall).onEntry(writeDirtyCnt := 0).whenIsActive {
      io.memPort.aw.setIdle()
      val w = io.memPort.w
      val toEvictLine = getCacheLine(whichWayToEvict, fsmIndex)
      when(writeDirtyCnt <= 16 - 1) {
        w.data := toEvictLine.data(writeDirtyCnt)
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
    }

    writeWaitB.onEntry(fsmNeedStall := True).onEntry(io.corePort.needStall := fsmNeedStall).whenIsActive {
      val b = io.memPort.b
      b.ready := True
      when(b.valid) {
        /* TODO: 处理异常 */
        when(b.resp === Axi4.resp.OKAY) {
          b.setIdle()
          goto(readCacheLine)
        }
      }
    }

    readCacheLine.onEntry(fsmNeedStall := True).onEntry(io.corePort.needStall := fsmNeedStall).onEntry(readStartFlag := False).onEntry(readCnt := 0).whenIsActive {
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
        ar.setIdle()
        readStartFlag := True
      }

      when(readStartFlag) {
        r.id := 0
        r.ready := True
        when(r.valid) {
          /* TODO: 处理异常 */
          when(r.resp === Axi4.resp.OKAY) {
            val line = getCacheLine(whichWayToEvict, fsmIndex)
            line.valid := True
            line.dirty := False
            line.tag := fsmTag
            line.counter := 0
            line.data(readCnt) := r.data

            val group = cacheMemories(index)
            group.subdivideIn(ways slices)(whichWayToEvict) := line.asBits
            cacheMemories.write(index, group)

            readCnt := readCnt + 1
          }
          when(r.last && readCnt === 16 - 1) {
            r.setIdle()
            goto(finish)
          }
        }
      }
    }

    finish.onEntry(fsmNeedStall := False).onEntry(io.corePort.needStall := False).whenIsActive {
      val line = getCacheLine(whichWayToEvict, fsmIndex)
      val group = cacheMemories(fsmIndex)
      group.subdivideIn(ways slices)(whichWayToEvict) := line.asBits

      when(fsmIsWrite) {
        line.data(fsmOffsetByWord) := fsmWriteValue
        line.dirty := True
      } otherwise {
        io.corePort.readValue := line.data(fsmOffsetByWord)
      }

      line.counter := line.counter + 1
      cacheMemories.write(fsmIndex, group)
      goto(cacheNormalWorking)
    }
  }
}
