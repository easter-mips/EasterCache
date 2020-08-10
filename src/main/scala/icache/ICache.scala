package icache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import lru.LruMem
import mem.BlockMem
import types._

class ICache(val config: CacheConfig, val transNum: Int) extends Module {
  val transNumWidth = log2Ceil(transNum)

  val io = IO(new Bundle {
    // interface to CPU
    val enable = Input(Bool())
    val iAddr = Input(UInt(32.W))
    val inst1 = Output(UInt(32.W))
    val inst2 = Output(UInt(32.W))
    val inst1Valid = Output(Bool())
    val inst2Valid = Output(Bool())
    val action = Input(UInt(4.W))
    // interface to AXI ram
    val axiReadAddrOut = Output(new AxiReadAddrOut)
    val axiReadAddrIn = Input(new AxiReadAddrIn)
    val axiReadOut = Output(new AxiReadOut)
    val axiReadIn = Input(new AxiReadIn)
    // hit stats
    val hitStats = Output(new HitStats)
  })

  // helper functions
  val fuse: (UInt, UInt) => UInt = (w: UInt, i: UInt) => Cat(w, i)

  def getMask(w: Int, n: UInt): UInt = {
    (1.U << n).asUInt.pad(w)
  }

  def setBit(x: UInt, n: UInt): UInt = {
    val w = x.getWidth
    x | getMask(w, n)
  }

  def clearBit(x: UInt, n: UInt): UInt = {
    val m: UInt = getMask(x.getWidth, n)
    x & (~m).asUInt
  }

  // data mem
  val dataMem = List.fill(config.wayNum) {
    Module(new BlockMem(new MemConfig(
      depth = config.lineNums,
      lineSize = config.lineBankNum
    )))
  }
  val rDataMem = VecInit(dataMem.map { _.io.dataOut })
  val wDataMem = Wire(Vec(config.lineBankNum, UInt(32.W)))
  val dataMemAddr = Wire(UInt(32.W))
  dataMem.foreach { m =>
    m.io.addr := dataMemAddr
    m.io.dataIn := wDataMem
  }

  // control state
  val validMem = RegInit(VecInit.tabulate(config.wayNum) { _ => 0.U(config.lineNums.W) } )
  val tagMem = Mem(config.wayNum * config.lineNums, UInt(config.tagWidth.W))
  val lruMem = Module(new LruMem(config))

  // axi state
  // axi state def
  val rsIdle :: rsAddressing :: rsRead :: rsRefill :: Nil = Enum(4)

  val rState = RegInit(VecInit.tabulate(transNum) { _ => rsIdle })
  val rAddr = RegInit(VecInit.tabulate(transNum) { _ => 0.U(32.W) })
  val rBank = RegInit(VecInit.tabulate(transNum) { _ => 0.U(config.bankNumWidth.W) })
  val rBuf = List.fill(transNum) { Mem(config.lineBankNum, UInt(32.W)) }
  val rValid = RegInit(VecInit.tabulate(transNum) { _ => 0.U(config.lineBankNum.W) })
  val rRefillWay = RegInit(VecInit.tabulate(transNum) { _ => 0.U(config.wayNumWidth.W) })
  val rInvalid = RegInit(VecInit.tabulate(transNum) { _ => false.B })

  val rBufData = VecInit.tabulate(transNum) { i => VecInit.tabulate(config.lineBankNum) { j => rBuf(i)(j) } }

  def isState(s: UInt) = VecInit.tabulate(transNum) { i => rState(i) === s }

  val rIsIdle = Wire(Vec(transNum, Bool()))
  rIsIdle := isState(rsIdle)
  val rIsAddressing = Wire(Vec(transNum, Bool()))
  rIsAddressing := isState(rsAddressing)
  val rIsRefilling = Wire(Vec(transNum, Bool()))
  rIsRefilling := isState(rsRefill)

  val idleSel = Wire(UInt(transNumWidth.W))
  idleSel := PriorityEncoder(rIsIdle)
  val addressingSel = Wire(UInt(transNumWidth.W))
  addressingSel := PriorityEncoder(rIsAddressing)
  val refillSel = Wire(UInt(transNumWidth.W))
  refillSel := PriorityEncoder(rIsRefilling)

  val axiRValid = Wire(UInt(transNum.W))
  axiRValid := VecInit.tabulate(transNum) { i =>
    io.axiReadIn.rvalid && io.axiReadIn.rid === i.U
  }.asUInt

  // output state
  val osNone :: osKnown1 :: osKnown2 :: osRead :: Nil = Enum(4)
  val oState = RegInit(osNone)
  val oKnownInst1 = RegInit(0.U(32.W))
  val oKnownInst2 = RegInit(0.U(32.W))
  val oReadWay = RegInit(0.U(config.wayNumWidth.W))
  val oReadBank = RegInit(0.U(config.bankNumWidth.W))

  // hit state
  val hitCount = RegInit(0.U(32.W))
  val missCount = RegInit(0.U(32.W))
  val prevAddr = RegInit(0.U(32.W))

  // wire defs
  val iTag = Wire(UInt(config.tagWidth.W))
  val iSet = Wire(UInt(config.setWidth.W))
  val iBank = Wire(UInt(config.bankNumWidth.W))
  iTag := config.sliceTag(io.iAddr)
  iSet := config.sliceSet(io.iAddr)
  iBank := config.sliceBank(io.iAddr)

  // defualt output
  io.inst1 := 0.U
  io.inst2 := 0.U
  io.inst1Valid := false.B
  io.inst2Valid := false.B
  io.axiReadAddrOut.arid := addressingSel.pad(4)
  io.axiReadAddrOut.araddr := rAddr(addressingSel)
  io.axiReadAddrOut.arvalid := rIsAddressing.asUInt.orR
  io.axiReadAddrOut.arlen := 7.U
  io.axiReadAddrOut.arsize := 2.U
  io.axiReadAddrOut.arburst := 2.U
  io.axiReadOut.rready := isState(rsRead).asUInt.orR
  io.hitStats.hitCount := hitCount
  io.hitStats.missCount := missCount

  dataMemAddr := Mux(rIsRefilling.asUInt.orR, config.sliceSet(rAddr(refillSel)), iSet)
  for (i <- 0 until config.lineBankNum) {
    wDataMem(i) := rBufData(refillSel)(i)
  }

  dataMem.indices.foreach { i =>
    dataMem(i).io.wEn := i.U === rRefillWay(refillSel) && rState(refillSel) === rsRefill
  }

  lruMem.io.setAddr := iSet
  lruMem.io.visit := 0.U
  lruMem.io.visitValid := false.B

  when (io.enable) { prevAddr := io.iAddr }

  // output handle
  switch (oState) {
    is(osNone) {
      io.inst1 := 0.U
      io.inst2 := 0.U
    }
    is(osKnown1) {
      io.inst1 := oKnownInst1
      io.inst2 := 0.U
    }
    is(osKnown2) {
      io.inst1 := oKnownInst1
      io.inst2 := oKnownInst2
    }
    is(osRead) {
      io.inst1 := rDataMem(oReadWay)(oReadBank)
      io.inst2 := rDataMem(oReadWay)(oReadBank + 1.U)
    }
  }

  // hit handle
  // hit cache data
  val hitWays = Wire(UInt(config.wayNum.W))
  hitWays := VecInit.tabulate(config.wayNum) { i =>
    tagMem(fuse(i.U(config.wayNumWidth.W), iSet)) === iTag && validMem(i.U(config.wayNumWidth.W))(iSet)
  }.asUInt
  val hitWay = Wire(Bool())
  hitWay := hitWays.orR() && !rIsRefilling.asUInt.orR
  val hitWayId = Wire(UInt(config.wayNumWidth.W))
  hitWayId := OHToUInt(hitWays)
  // whether input addr is in a axi transaction
  val inAxiReads = Wire(UInt(transNum.W))
  inAxiReads := VecInit.tabulate(transNum) { i =>
    config.sliceLineAddr(io.iAddr) === config.sliceLineAddr(rAddr(i)) &&
      rState(i) =/= rsIdle
  }.asUInt
  val inAxiRead = Wire(Bool())
  inAxiRead := inAxiReads.orR
  // hit axi direct
  val hitAxiDirects = Wire(UInt(transNum.W))
  hitAxiDirects := VecInit.tabulate(transNum) { i =>
    inAxiReads(i) && iBank === rBank(i) && axiRValid(i) && rState(i) === rsRead
  }.asUInt
  val hitAxiDirect = Wire(Bool())
  hitAxiDirect := hitAxiDirects.orR()
  val hitAxiDirectId = Wire(UInt(transNumWidth.W))
  hitAxiDirectId := PriorityEncoder(hitAxiDirects)
  // hit axi buf
  val hitAxiBufs = Wire(UInt(transNumWidth.W))
  hitAxiBufs := VecInit.tabulate(transNum) { i =>
    inAxiReads(i) && rValid(i)(iBank) && (rState(i) === rsRead || rState(i) === rsRefill)
  }.asUInt
  val hitAxiBuf = Wire(Bool())
  hitAxiBuf := hitAxiBufs.orR
  val hitAxiBufId = Wire(UInt(transNumWidth.W))
  hitAxiBufId := PriorityEncoder(hitAxiBufs)
  // invalid addr
  val invalidAddr = Wire(Bool())
  invalidAddr := io.iAddr(1, 0) =/= 0.U

  lruMem.io.visitValid := hitAxiDirect || hitAxiBuf || hitWay

  when (io.enable && io.iAddr =/= prevAddr) {
    when (hitAxiDirect || hitAxiBuf || hitWay || invalidAddr) {
      hitCount := hitCount + 1.U
    } .otherwise {
      missCount := missCount + 1.U
    }
  }

  // cache action
  val actionAddr = Wire(Bool())
  val actionInv = Wire(Bool())
  val actionValid = Wire(Bool())
  actionAddr := io.action(3)
  actionInv := io.action(2)
  actionValid := io.action(2, 0).orR

  when (actionInv) { // invalidate
    when (actionAddr === 0.U && hitWay) { // hit invalidate
      validMem(hitWayId) := clearBit(validMem(hitWayId), iSet)
    } .elsewhen(actionAddr === 0.U && (hitAxiDirect || hitAxiBuf)) {
      rInvalid(Mux(hitAxiDirect, hitAxiDirectId, hitAxiBufId)) := true.B
    }
    when (actionAddr === 1.U) { // index invalidate
      // get way id
      val invWayId = Wire(UInt(config.wayNumWidth.W))
      invWayId := iTag(config.wayNumWidth - 1, 0)
      validMem(invWayId) := clearBit(validMem(invWayId), iSet)
    }
  }

  when (io.enable && !actionValid) {
    when (invalidAddr) {
      io.inst1Valid := true.B
      io.inst2Valid := true.B
      oState := osKnown2
      oKnownInst1 := 0.U
      oKnownInst2 := 0.U
    }
    when (hitAxiDirect) {
      io.inst1Valid := true.B
      oState := osKnown1
      oKnownInst1 := io.axiReadIn.rdata
      lruMem.io.visit := rRefillWay(hitAxiDirectId)
    } .elsewhen (hitAxiBuf) {
      io.inst1Valid := true.B
      io.inst2Valid := iBank =/= 7.U && rValid(hitAxiBufId)(iBank + 1.U)
      oState := osKnown2
      oKnownInst1 := rBufData(hitAxiBufId)(iBank)
      oKnownInst2 := rBufData(hitAxiBufId)(iBank + 1.U)
      lruMem.io.visit := rRefillWay(hitAxiBufId)
    } .elsewhen (hitWay) {
      io.inst1Valid := true.B
      io.inst2Valid := iBank =/= 7.U
      oState := osRead
      oReadWay := hitWayId
      oReadBank := iBank
      lruMem.io.visit := hitWayId
    } .otherwise {
      // miss handle
      oState := osNone
      when (!inAxiRead.orR && rIsIdle.asUInt.orR && !rIsAddressing.asUInt.orR) {
        rState(idleSel) := rsAddressing
        rAddr(idleSel) := io.iAddr
        rBank(idleSel) := iBank
        rValid(idleSel) := 0.U
        rInvalid(idleSel) := false.B
        rRefillWay(idleSel) := lruMem.io.waySel
      }
    }
  }

  // axi read handle
  (0 until transNum).foreach { i: Int =>
    switch(rState(i)) {
      is (rsAddressing) {
        when (i.U === addressingSel && io.axiReadAddrIn.arready) {
          rState(i) := rsRead
        }
      }
      is (rsRead) {
        rBuf(i)(rBank(i)) := io.axiReadIn.rdata
        when (axiRValid(i)) {
          rState(i) := Mux(io.axiReadIn.rlast, rsRefill, rsRead)
          rValid(i) := setBit(rValid(i), rBank(i))
          rBank(i) := rBank(i) + 1.U
        }
      }
      is(rsRefill) {
        when (i.U === refillSel) {
          rState(i) := rsIdle
          tagMem(fuse(rRefillWay(i), config.sliceSet(rAddr(i)))) := config.sliceTag(rAddr(i))
          validMem(rRefillWay(i)) := Mux(
            rInvalid(i),
            clearBit(validMem(rRefillWay(i)), config.sliceSet(rAddr(i))),
            setBit(validMem(rRefillWay(i)), config.sliceSet(rAddr(i)))
          )
        }
      }
    }
  }
}

object ICache extends App {
  new ChiselStage execute(args, Seq(ChiselGeneratorAnnotation(
    () =>
      new ICache(new CacheConfig(wayNum = 2, setWidth = 8), transNum = 4))))
}
