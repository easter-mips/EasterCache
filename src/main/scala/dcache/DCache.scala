// See README.md for license details.
package dcache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import scala.collection.immutable.List

class AxiReadAddrOut extends Bundle {
  val arid = UInt(4.W)
  val araddr = UInt(32.W)
  val arvalid = Bool()
  val arlen = UInt(4.W)
  val arsize = UInt(3.W)
  val arburst = UInt(2.W)
}

class AxiReadAddrIn extends Bundle {
  val arready = Bool()
}

class AxiReadOut extends Bundle {
  val rready = Bool()
}

class AxiReadIn extends Bundle {
  val rid = UInt(4.W)
  val rdata = UInt(32.W)
  val rresp = UInt(2.W)
  val rlast = Bool()
  val rvalid = Bool()
}

class CacheConfig(val wayNum: Int, val setWidth: Int) {
  val tagWidth = 32 - 5 - setWidth
  val wayNumWidth = log2Ceil(wayNum)
  val lineAddrWidth = 32 - 5
  val lineNums = List.fill(setWidth)(2).foldLeft(1)(_ * _)

  def sliceTag(x: UInt): UInt = x(31, 32 - tagWidth)
  def sliceSet(x: UInt): UInt = x(31 - tagWidth, 32 - tagWidth - setWidth)
  def sliceBank(x: UInt): UInt = x(4, 2)
  def sliceLineAddr(x: UInt): UInt = x(31, 5)
}

class DCache(config: CacheConfig) extends Module {
  val lineBankNum = 8
  val bankSelWidth = 3 // 3 = log2(8)

  def expandStrb(wstrb: UInt): UInt = {
    val strbW = wstrb.getWidth
    (3 to 0 by -1).map(i => Mux(wstrb(i), "hff".U(8.W), 0.U(8.W))).foldLeft(0.U(1.W))((acc, x) => Cat(acc, x))(strbW * 8 - 1, 0)
  }

  def writeWord(word: UInt, wstrb: UInt, wdata: UInt): UInt = {
    val wMask = Wire(UInt(32.W))
    wMask := expandStrb(wstrb)
    (wMask & wdata) | ((~wMask) & word)
  }

  def readWord(word: UInt, dAddr: UInt, dSize: UInt): UInt = {
    val rData = Wire(UInt(32.W))
    rData := 0.U
    when (dSize === 0.U) {
      switch (dAddr(1, 0)) {
        is(0.U) {
          rData := Cat(0.U(24.W), word(7, 0))
        }
        is(1.U) {
          rData := Cat(0.U(24.W), word(15, 8))
        }
        is(2.U) {
          rData := Cat(0.U(24.W), word(23, 16))
        }
        is(3.U) {
          rData := Cat(0.U(24.W), word(31, 24))
        }
      }
    } .elsewhen(dSize === 1.U) {
      when (dAddr(1) === 0.U) {
        rData := Cat(0.U(16.W), word(15, 0))
      } .otherwise {
        rData := Cat(0.U(16.W), word(31, 16))
      }
    } .otherwise {
      rData := word
    }
    rData
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
    // bank data interface
    val bankDataIn = Input(Vec(config.wayNum, Vec(lineBankNum, UInt(32.W))))
    val bankDataOut = Output(Vec(lineBankNum, UInt(32.W)))
    val bankWEn = Output(Vec(config.wayNum, Vec(lineBankNum, UInt(4.W))))
    val bankSetAddr = Output(UInt(32.W))
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
  val rsIdle :: rsAddr :: rsRead :: rsRefill :: Nil = Enum(4)
  // victim cache write state
  val wsIdle :: wsRead :: wsSubmit :: Nil = Enum(3)
  // output state
  val osNone :: osKnown :: osRead :: Nil = Enum(3)

  // control state
  def initLineState(x: UInt) = RegInit(VecInit(List.fill(config.wayNum)(VecInit(List.fill(config.lineNums)(x)))))
  val tagMem = initLineState(0.U(config.tagWidth.W))
  val validMem = initLineState(0.U(1.W))
  val dirtyMem = initLineState(0.U(1.W))
  val lruMem = RegInit(VecInit(List.fill(config.lineNums)(0.U(getLruWidth(config.wayNum).W))))

  // axi states
  val rState = RegInit(rsIdle)
  val wState = RegInit(wsIdle)

  val rAddr = RegInit(0.U(32.W))
  val rBank = RegInit(0.U(3.W))
  val rBuf = RegInit(VecInit(List.fill(lineBankNum)(0.U(32.W))))
  val rValid = RegInit(VecInit(List.fill(lineBankNum)(false.B)))
  val rRefillSel = RegInit(0.U(config.wayNumWidth.W))
  val rDirty = RegInit(false.B)

  val wAddr = RegInit(0.U(32.W))
  val wBuf = RegInit(VecInit(List.fill(lineBankNum)(0.U(32.W))))
  val wNeedWB = RegInit(false.B)

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
  switch (oState) {
    is(osKnown) {
      io.rData := oKnownData
    }

    is(osRead) {
      io.rData := io.bankDataIn(oReadWay)(oReadBank)
      io.rData := readWord(io.bankDataIn(oReadWay)(oReadBank), oReadAddr, oReadSize)
    }
  }

  // bank write data
  val hitBankOut = Wire(Vec(lineBankNum, UInt(32.W)))
  hitBankOut := VecInit((0 until lineBankNum).map(_ => io.wData))
  io.bankDataOut := Mux(rState === rsRefill, rBuf, hitBankOut)

  // whether axi is ready
  val axiReady = Wire(Bool())
  axiReady := rState === rsIdle && wState === wsIdle

  // hit handle
  val hitWays = Wire(UInt(config.wayNum.W))
  hitWays := VecInit((0 until config.wayNum).map(i => validMem(i)(dSet).asBool && (tagMem(i)(dSet) === dTag))).asUInt
  val hitWay = Wire(Bool())
  hitWay := hitWays.orR()
  val hitWayId = Wire(UInt(config.wayNumWidth.W))
//  hitWayId := (0 until config.wayNum).map(i => Mux(hitWays(i), i.U(config.wayNumWidth), 0.U))
//    .foldLeft(0.U)((acc, x) => acc | x)
  hitWayId := PriorityEncoder(hitWays)

  val hitAxiBuf = Wire(Bool())
  hitAxiBuf := config.sliceLineAddr(rAddr) === config.sliceLineAddr(io.dAddr) && rState === rsRead && rValid(config.sliceBank(io.dAddr))
  val hitAxiDirect = Wire(Bool())
  hitAxiDirect := config.sliceLineAddr(rAddr) === config.sliceLineAddr(io.dAddr) && rState === rsRead &&
    config.sliceBank(io.dAddr) === rBank && io.axiReadIn.rvalid
  // conflicting condition I: cache is refilling data back into BRAM
  val refilling = Wire(Bool())
  refilling := rState === rsRefill
  // conflicting condition II: requested line is being written back to AXI ram
  val axiWritingBack = Wire(Bool())
  axiWritingBack := config.sliceLineAddr(io.dAddr) === config.sliceLineAddr(wAddr) && !axiReady && wNeedWB

  val hit = Wire(Bool())
  hit := !axiWritingBack && !refilling && (hitAxiDirect || hitAxiBuf || hitWay)
  io.dWait := io.enable && !hit

  // lru fsm
  val lruFsm = Module(new LruFsm(config.wayNum))
  lruFsm.io.current := lruMem(dSet)

  val refillSel = Wire(UInt(config.wayNumWidth.W))
  refillSel := lruFsm.io.sel

  lruFsm.io.visit := Mux(hit, hitWayId, refillSel)

  when (io.enable && !axiWritingBack && !refilling) {
    when (hitAxiDirect) {
      oState := osKnown
      oKnownData := readWord(io.axiReadIn.rdata, io.dAddr, io.dSize)
      rDirty := Mux(io.wEn, 1.U, rDirty)
    } .elsewhen(hitAxiBuf) {
      oState := osKnown
      oKnownData := readWord(rBuf(dBank), io.dAddr, io.dSize)
      rDirty := Mux(io.wEn, 1.U, rDirty)
      rBuf(dBank) := writeWord(rBuf(dBank), io.wStrb, io.wData)
    } .elsewhen(hitWay) {
      oState := osRead
      oReadWay := hitWayId
      oReadBank := dBank
      oReadAddr := io.dAddr
      oReadSize := io.dSize
      // output bank write enable
      io.bankWEn(hitWayId)(dBank) := io.wStrb
      // update control
      dirtyMem(hitWayId)(dSet) := Mux(io.wEn, 1.U, rDirty)
      lruMem(dSet) := lruFsm.io.next
    } .elsewhen(axiReady) {
      // miss
      rState := rsAddr
      rAddr := Cat(io.dAddr(31, 2), "b00".U(2.W))
      rBank := dBank
      rValid := VecInit(List.fill(lineBankNum)(false.B))
      rDirty := 0.U
      rRefillSel := refillSel

      wNeedWB := validMem(refillSel)(dSet)
      wState := Mux(validMem(refillSel)(dSet).asBool(), wsRead, wsIdle)
      wAddr := Cat(tagMem(rRefillSel)(dSet), Cat(dSet, 0.U(5.W)))

      // update lru
      lruMem(dSet) := lruFsm.io.next
    }
  }

  // axi state transition
  // write state
  switch (wState) {
    is(wsRead) {
      wState := wsSubmit
      wBuf := io.bankDataIn(rRefillSel)
    }

    is(wsSubmit) {
      wState := Mux(io.vcReady, wsIdle, wsSubmit)
    }
  }
  // read state
  switch (rState) {
    is(rsAddr) {
      when (io.vcHit) {
        rState := rsRefill
        rValid := VecInit(List.fill(lineBankNum)(true.B))
        rBuf := io.vcRData
      } .otherwise {
        io.axiReadAddrOut.arvalid := 1.U
        rState := Mux(io.axiReadAddrIn.arready, rsRead, rsAddr)
      }
    }

    is(rsRead) {
      when (hitAxiDirect && io.wEn) {
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
      tagMem(rRefillSel)(config.sliceSet(rAddr)) := config.sliceTag(rAddr)
      validMem(rRefillSel)(config.sliceSet(rAddr)) := 1.U
      dirtyMem(rRefillSel)(config.sliceSet(rAddr)) := rDirty

      rState := rsIdle
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
  (new ChiselStage)execute(args, Seq(ChiselGeneratorAnnotation(
    () =>
      new DCache(new CacheConfig(wayNum = 4, setWidth = 7)))))
}
