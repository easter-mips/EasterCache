// See README.md for license details.
package dcache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import lru.LruMem
import types._

import scala.collection.immutable.List

class DCache(config: CacheConfig, verbose: Boolean = false) extends Module {
  val lineBankNum = 8
  val bankSelWidth = 3 // 3 = log2(8)

  /**
    * Expand wstrb to bit-wise masks.
    * Example:
    * expandStrb("b0010".U) === "h00_00_ff_00".U
    *
    * @param wstrb
    * @return corresponding bitwise mask of wstrb
    */
  def expandStrb(wstrb: UInt): UInt = {
    val strbW = wstrb.getWidth
    (3 to 0 by -1).map(i => Mux(wstrb(i), "hff".U(8.W), 0.U(8.W))).foldLeft(0.U(1.W))((acc, x) => Cat(acc, x))(strbW * 8 - 1, 0)
  }

  /**
    * Change input 32-bit word according to wstrb and wdata
    *
    * @param word  the input word
    * @param wstrb write mask
    * @param wdata write data
    * @return modified word
    */
  def writeWord(word: UInt, wstrb: UInt, wdata: UInt): UInt = {
    val wMask = Wire(UInt(32.W))
    wMask := expandStrb(wstrb)
    (wMask & wdata) | ((~wMask) & word)
  }

//  /**
//    * Get output based on address and read size
//    *
//    * @param word
//    * @param dAddr
//    * @param dSize
//    * @return
//    */
//  def readWord(word: UInt, dAddr: UInt, dSize: UInt): UInt = {
//    val rData = Wire(UInt(32.W))
//    rData := 0.U
//    when(dSize === 0.U) {
//      switch(dAddr(1, 0)) {
//        is(0.U) {
//          rData := Cat(0.U(24.W), word(7, 0))
//        }
//        is(1.U) {
//          rData := Cat(0.U(24.W), word(15, 8))
//        }
//        is(2.U) {
//          rData := Cat(0.U(24.W), word(23, 16))
//        }
//        is(3.U) {
//          rData := Cat(0.U(24.W), word(31, 24))
//        }
//      }
//    }.elsewhen(dSize === 1.U) {
//      when(dAddr(1) === 0.U) {
//        rData := Cat(0.U(16.W), word(15, 0))
//      }.otherwise {
//        rData := Cat(0.U(16.W), word(31, 16))
//      }
//    }.otherwise {
//      rData := word
//    }
//    rData
//  }

  /**
    * Get a mask with its n-th bit being 1 of width w
    *
    * @param w
    * @param n
    * @return
    */
  def getMask(w: Int, n: UInt): UInt = {
    (1.U << n).asUInt.pad(w)
  }

  /**
    * Set the n-th bit of x to 1
    *
    * @param x
    * @param n
    * @return
    */
  def setBit(x: UInt, n: UInt): UInt = {
    val w = x.getWidth
    x | getMask(w, n)
  }

  /**
    * Set the n-th bit of x to 0
    *
    * @param x
    * @param n
    * @return
    */
  def clearBit(x: UInt, n: UInt): UInt = {
    val m: UInt = getMask(x.getWidth, n)
    x & (~m).asUInt
  }

  val io = IO(new Bundle {
    // cpu interface
    val enable = Input(Bool())
    val dAddr = Input(UInt(32.W))
    val dSize = Input(UInt(3.W))
    val wData = Input(UInt(32.W))
    val wEn = Input(Bool())
    val wStrb = Input(UInt(4.W))
    val dWait = Output(Bool())
    val rData = Output(UInt(32.W))
    val action = Input(UInt(4.W))
    // statistic interface
    val missCount = Output(UInt(32.W))
    val hitCount = Output(UInt(32.W))
    // bank data interface
    val bankDataIn = Input(Vec(config.wayNum, Vec(lineBankNum, UInt(32.W))))
    val bankDataOut = Output(Vec(lineBankNum, UInt(32.W)))
    val bankWEn = Output(Vec(config.wayNum, Vec(lineBankNum, UInt(4.W))))
    val bankSetAddr = Output(UInt(config.setWidth.W))
    // axi interface
    val axiReadAddrOut = Output(new AxiReadAddrOut)
    val axiReadAddrIn = Input(new AxiReadAddrIn)
    val axiReadOut = Output(new AxiReadOut)
    val axiReadIn = Input(new AxiReadIn)
    // victim cache interface
    val vcReady = Input(Bool())
    val vcHit = Input(Bool())
    val vcWAddr = Output(UInt(config.lineAddrWidth.W))
    val vcWData = Output(Vec(lineBankNum, UInt(32.W)))
    val vcWValid = Output(Bool())
    val vcRAddr = Output(UInt(config.lineAddrWidth.W))
    val vcRData = Input(Vec(lineBankNum, UInt(32.W)))
  })

  // axi read state
  val rsIdle :: rsAddr :: rsRead :: rsRefill :: rsWB :: Nil = Enum(5)
  // victim cache write state
  val wsIdle :: wsRead :: wsSubmit :: Nil = Enum(3)
  // output state
  val osNone :: osKnown :: osRead :: Nil = Enum(3)

  // control state
  /**
    * tag memory
    * Size: (way number * line number per way) * tag width
    * Address width: way number width + set width
    */
  val tagMem = Mem(config.wayNum * config.lineNums, UInt(config.tagWidth.W))
  val validMem = RegInit(VecInit(List.fill(config.wayNum)(0.U(config.lineNums.W))))
  val dirtyMem = RegInit(VecInit(List.fill(config.wayNum)(0.U(config.lineNums.W))))

  // statistics
  val hitCount = RegInit(0.U(32.W))
  val missCount = RegInit(0.U(32.W))
  val prevAddr = RegInit(0.U(32.W)) // do not count stalled requests multiple times

  prevAddr := Mux(io.enable, io.dAddr, prevAddr)
  io.hitCount := hitCount
  io.missCount := missCount

  // helper function
  // Example:
  /**
    * fuse way number and set address into one UInt to address the mem
    * Example:
    * To address the tag of set 10 at way 2, use:
    * tagMem(fuse(2.U, 10.U))
    */
  val fuse: (UInt, UInt) => UInt = (wid, sid) => Cat(wid, sid)

  // axi states
  val rState = RegInit(rsIdle)
  val wState = RegInit(wsIdle)

  // axi read registers
  // the requested axi addr
  val rAddr = RegInit(0.U(32.W))
  // next bank to read from axi
  val rBank = RegInit(0.U(3.W))
  // read buffer
  val rBuf = RegInit(VecInit(List.fill(lineBankNum)(0.U(32.W))))
  // valid indicator of buffer
  val rValid = RegInit(VecInit(List.fill(lineBankNum)(false.B)))
  // the way to refill
  val rRefillSel = RegInit(0.U(config.wayNumWidth.W))
  // whether read out data is dirty
  val rDirty = RegInit(false.B)
  val rInvalid = RegInit(false.B)
  val rWB = RegInit(false.B)

  // axi write registers
  // the address to write back
  val wAddr = RegInit(0.U(32.W))
  // write buffer
  val wBuf = RegInit(VecInit(List.fill(lineBankNum)(0.U(32.W))))
  // the swapped out line needs to be written back
  val wNeedWB = RegInit(false.B)

  // write request buffer
  val wReqBank = RegInit(0.U(config.bankNumWidth.W))
  val wReqData = RegInit(0.U(32.W))
  val wReqWStrb = RegInit(0.U(4.W))
  val wReqValid = RegInit(false.B)

  // wire defs
  val dSet = Wire(UInt(config.setWidth.W))
  dSet := config.sliceSet(io.dAddr)
  val dTag = Wire(UInt(config.tagWidth.W))
  dTag := config.sliceTag(io.dAddr)
  val dBank = Wire(UInt(bankSelWidth.W))
  dBank := config.sliceBank(io.dAddr)

  // output states
  val oState = RegInit(osNone)
  val oKnownData = RegInit(0.U(32.W))
  val oReadWay = RegInit(0.U(config.wayNumWidth.W))
  val oReadBank = RegInit(0.U(bankSelWidth.W))
  val oReadAddr = RegInit(0.U(32.W))
  val oReadSize = RegInit(0.U(3.W))

  // default output
  def fillBankVec(x: UInt): Vec[Vec[UInt]] = VecInit(List.fill(config.wayNum)(VecInit(List.fill(lineBankNum)(x))))

  io.bankWEn := fillBankVec(0.U(1.W))
  io.bankSetAddr := Mux(rState === rsRefill, config.sliceSet(rAddr), dSet)
  io.axiReadAddrOut.arid := 0.U
  io.axiReadAddrOut.araddr := rAddr
  io.axiReadAddrOut.arvalid := 0.U
  io.axiReadAddrOut.arlen := 7.U
  io.axiReadAddrOut.arsize := 2.U
  io.axiReadAddrOut.arburst := 2.U
  io.axiReadOut.rready := rState === rsRead
  io.vcWAddr := config.sliceLineAddr(wAddr)
  io.vcWData := wBuf
  io.vcWValid := wState === wsSubmit
  io.vcRAddr := config.sliceLineAddr(io.dAddr)

  // output handle
  io.rData := 0.U
  switch(oState) {
    is(osKnown) {
      io.rData := oKnownData
    }
    is(osRead) {
//      io.rData := readWord(io.bankDataIn(oReadWay)(oReadBank), oReadAddr, oReadSize)
      io.rData := io.bankDataIn(oReadWay)(oReadBank)
    }
  }

  // bank write data
  val hitBankOut = Wire(Vec(lineBankNum, UInt(32.W)))
  hitBankOut := VecInit((0 until lineBankNum).map(_ => io.wData))
  io.bankDataOut := Mux(rState === rsRefill, rBuf, hitBankOut)

  // whether axi is ready
  val axiReady = Wire(Bool())
  axiReady := rState === rsIdle && wState === wsIdle

  // whether possible to buffer the write request
  val canBuffer = Wire(Bool())
  canBuffer := !wReqValid && io.wEn && axiReady

  // hit handle
  val hitWays = Wire(UInt(config.wayNum.W))
  hitWays := VecInit((0 until config.wayNum).map(i => validMem(i)(dSet) && (tagMem(fuse(i.U(config.wayNumWidth.W), dSet)) === dTag))).asUInt
  val hitWay = Wire(Bool())
  hitWay := hitWays.orR()
  val hitWayId = Wire(UInt(config.wayNumWidth.W))
  hitWayId := PriorityEncoder(hitWays)

  // buffered write request hit axi read
  val bufHitAxiDirect = Wire(Bool())
  bufHitAxiDirect := wReqValid && wReqBank === rBank && rState === rsRead && io.axiReadIn.rvalid

  val hitAxiBuf = Wire(Bool())
  hitAxiBuf := config.sliceLineAddr(rAddr) === config.sliceLineAddr(io.dAddr) && rState === rsRead && rValid(config.sliceBank(io.dAddr))
  val hitAxiDirect = Wire(Bool())
  hitAxiDirect := config.sliceLineAddr(rAddr) === config.sliceLineAddr(io.dAddr) && rState === rsRead &&
    config.sliceBank(io.dAddr) === rBank && io.axiReadIn.rvalid && !bufHitAxiDirect
  // conflicting condition I: cache is refilling data back into BRAM
  val refilling = Wire(Bool())
  refilling := rState === rsRefill
  // conflicting condition II: requested line is being written back to AXI ram
  val axiWritingBack = Wire(Bool())
  axiWritingBack := config.sliceLineAddr(io.dAddr) === config.sliceLineAddr(wAddr) && !axiReady && wNeedWB

  val hit = Wire(Bool())
  hit := !axiWritingBack && !refilling && (hitAxiDirect || hitAxiBuf || hitWay)

  // count miss & hit
  val isMiss = Wire(Bool())
  isMiss := io.dAddr =/= prevAddr && io.enable && !hit
  val isHit = Wire(Bool())
  isHit := io.dAddr =/= prevAddr && io.enable && hit
  missCount := Mux(isMiss, missCount + 1.U, missCount)
  hitCount := Mux(isHit, hitCount + 1.U, hitCount)

  val lruMem = Module(new LruMem(config))
  lruMem.io.setAddr := dSet
  lruMem.io.visit := 0.U
  lruMem.io.visitValid := hit

  val refillSel = Wire(UInt(config.wayNumWidth.W))
  refillSel := lruMem.io.waySel

  // will not synthesize: event logging
  if (verbose) {
    when(io.enable) {
      printf(p"requested address: ${io.dAddr}\n")
      when(axiWritingBack) {
        printf("stall reason: hitted line is being written back\n")
      }.elsewhen(refilling) {
        printf("stall reason: cache is refilling data\n")
      }.elsewhen(hitAxiDirect) {
        printf("hit axi direct\n")
      }.elsewhen(hitAxiBuf) {
        printf("hit axi read buffer\n")
      }.elsewhen(hitWay) {
        printf(p"hit bank data in way ${hitWayId}\n")
      }.otherwise {
        when(axiReady) {
          printf("miss, axi ready\n")
        }.otherwise {
          printf("miss, axi not ready\n")
        }
      }
    }
  }

  // cache action
  val actionAddr = Wire(Bool())
  val actionInv = Wire(Bool())
  val actionStore = Wire(Bool())
  val actionWB = Wire(Bool())
  val actionValid = Wire(Bool())
  actionAddr := io.action(3)
  actionInv := io.action(2)
  actionStore := io.action(1)
  actionWB := io.action(0)
  actionValid := actionInv || actionWB

  val actionOk = Wire(Bool())
  actionOk := true.B

  io.dWait := (io.enable && !hit && !canBuffer) || (actionValid && !actionOk)

  // invalidate
  when (actionInv) {
    when (actionAddr === 0.U) { // hit addressing
      when (hitAxiDirect || hitAxiBuf) {
        rInvalid := true.B
      } .elsewhen (hitWay) {
        validMem(hitWayId) := clearBit(validMem(hitWayId), dSet)
      }
    } .otherwise { // index addressing
      val invWayId = Wire(UInt(config.wayNumWidth.W))
      invWayId := dTag(config.wayNumWidth - 1, 0)
      validMem(invWayId) := clearBit(validMem(invWayId), dSet)
    }
  }
  // write back
  when (actionWB) {
    when (actionAddr === 0.U) { // hit addressing
      when ((hitAxiDirect || hitAxiBuf) && rDirty) {
        // need write back
        rWB := true.B
      } .elsewhen(hitWay) {
        when (wState === wsIdle && rState =/= rsRefill) {
          wState := wsRead
          wNeedWB := true.B
          wAddr := Cat(tagMem(fuse(hitWayId, dSet)), Cat(dSet, 0.U(5.W)))
        } .otherwise {
          actionOk := false.B
        }
      }
    } .otherwise { // index addressing
      val invWayId = Wire(UInt(config.wayNumWidth.W))
      invWayId := dTag(config.wayNumWidth - 1, 0)
      when (validMem(invWayId)(dSet) && dirtyMem(invWayId)(dSet)) { // need write back
        when (wState === wsIdle && rState =/= rsRefill) {
          wState := wsRead
          wNeedWB := true.B
          wAddr := Cat(tagMem(fuse(invWayId, dSet)), Cat(dSet, 0.U(5.W)))
        } .otherwise {
          actionOk := false.B
        }
      }
    }
  }

  when(io.enable && !axiWritingBack && !refilling && !actionValid) {
    when(hitAxiDirect) {
      oState := osKnown
//      oKnownData := readWord(io.axiReadIn.rdata, io.dAddr, io.dSize)
      oKnownData := io.axiReadIn.rdata
      rDirty := Mux(io.wEn, 1.U, rDirty)
      // update lru
      lruMem.io.visit := rRefillSel
    }.elsewhen(hitAxiBuf) {
      oState := osKnown
//      oKnownData := readWord(rBuf(dBank), io.dAddr, io.dSize)
      oKnownData := rBuf(dBank)
      rDirty := Mux(io.wEn, 1.U, rDirty)
      rBuf(dBank) := writeWord(rBuf(dBank), io.wStrb, io.wData)
      // update lru
      lruMem.io.visit := rRefillSel
    }.elsewhen(hitWay) {
      oState := osRead
      oReadWay := hitWayId
      oReadBank := dBank
      oReadAddr := io.dAddr
      oReadSize := io.dSize
      // output bank write enable
      io.bankWEn(hitWayId)(dBank) := io.wStrb
      // update control
      dirtyMem(hitWayId) := Mux(io.wEn, setBit(dirtyMem(hitWayId), dSet), dirtyMem(hitWayId))
      lruMem.io.visit := hitWayId
    } .elsewhen(axiReady) {
      // miss
      rState := rsAddr
      rAddr := Cat(io.dAddr(31, 2), "b00".U(2.W))
      rBank := dBank
      rValid := VecInit(List.fill(lineBankNum)(false.B))
      rDirty := 0.U
      rInvalid := false.B
      rWB := false.B
      rRefillSel := refillSel

      wNeedWB := validMem(refillSel)(dSet)
      wState := Mux(validMem(refillSel)(dSet), wsRead, wsIdle)
      wAddr := Cat(tagMem(fuse(refillSel, dSet)), Cat(dSet, 0.U(5.W)))
      // invalidate selected line
      validMem(refillSel) := clearBit(validMem(refillSel), dSet)

      // this is a buffered write request
      when (canBuffer) {
        wReqBank := dBank
        wReqData := io.wData
        wReqWStrb := io.wStrb
        wReqValid := true.B
      }
    }
  }

  // axi state transition
  // write state
  switch(wState) {
    is(wsRead) {
      wState := wsSubmit
      wBuf := io.bankDataIn(rRefillSel)
    }

    is(wsSubmit) {
      wState := Mux(io.vcReady, wsIdle, wsSubmit)
    }
  }
  // read state
  switch(rState) {
    is(rsAddr) {
      when(io.vcHit) {
        rState := rsRefill
        rValid := VecInit(List.fill(lineBankNum)(true.B))
        rBuf := io.vcRData
      }.otherwise {
        io.axiReadAddrOut.arvalid := 1.U
        rState := Mux(io.axiReadAddrIn.arready, rsRead, rsAddr)
      }
    }

    is(rsRead) {
      when (bufHitAxiDirect) {
        rBuf(rBank) := writeWord(io.axiReadIn.rdata, wReqWStrb, wReqData)
        wReqValid := false.B
      } .elsewhen (hitAxiDirect && io.wEn) {
        rBuf(rBank) := writeWord(io.axiReadIn.rdata, io.wStrb, io.wData)
      } .otherwise {
        rBuf(rBank) := Mux(io.axiReadIn.rvalid, io.axiReadIn.rdata, 0.U)
      }
      rValid(rBank) := io.axiReadIn.rvalid
      rBank := Mux(io.axiReadIn.rvalid, rBank + 1.U, rBank)
      rState := Mux(io.axiReadIn.rlast && io.axiReadIn.rvalid, rsRefill, rsRead)
    }

    is(rsRefill) {
      // output bank write enable
      io.bankWEn(rRefillSel) := VecInit(List.fill(lineBankNum)("hf".U))
      // update control state
      tagMem(fuse(rRefillSel, config.sliceSet(rAddr))) := config.sliceTag(rAddr)
      dirtyMem(rRefillSel) := Mux(
        rDirty,
        setBit(dirtyMem(rRefillSel), config.sliceSet(rAddr)),
        clearBit(dirtyMem(rRefillSel), config.sliceSet(rAddr))
      )
      validMem(rRefillSel) := Mux(
        rInvalid,
        clearBit(validMem(rRefillSel), config.sliceSet(rAddr)),
        setBit(validMem(rRefillSel), config.sliceSet(rAddr))
      )

      rState := Mux(rWB, rsWB, rsIdle)
    }

    is(rsWB) {
      rState := Mux(wState === wsIdle, rsIdle, rsWB)
      when (wState === wsIdle) {
        wState := wsSubmit
        wBuf := rBuf
        wNeedWB := true.B
        wAddr := Cat(config.sliceLineAddr(rAddr), Cat(dSet, 0.U(5.W)))
      }
    }
  }
}

object fact {
  def f(x: Int): Int = if (x <= 0) 1 else x * f(x - 1)

  def apply(x: Int): Int = f(x)
}

object getLruWidth {
  def apply(wayNum: Int): Int = log2Ceil(fact(wayNum))
}

object DCache extends App {
  (new ChiselStage) execute(args, Seq(ChiselGeneratorAnnotation(
    () =>
      new DCache(new CacheConfig(wayNum = 2, setWidth = 8), verbose = false))))
}
