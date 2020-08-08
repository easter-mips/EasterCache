package icache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import lru.LruMem
import mem.BlockMem
import types._

class ICache(val config: CacheConfig) extends Module {
  val io = IO(new Bundle {
    // interface to CPU
    val enable = Input(Bool())
    val iAddr = Input(UInt(32.W))
    val inst1 = Output(UInt(32.W))
    val inst2 = Output(UInt(32.W))
    val inst1Valid = Output(Bool())
    val inst2Valid = Output(Bool())
    // interface to AXI ram
    val axiReadAddrOut = Output(new AxiReadAddrOut)
    val axiReadAddrIn = Input(new AxiReadAddrIn)
    val axiReadOut = Output(new AxiReadOut)
    val axiReadIn = Input(new AxiReadIn)
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
  val rsIdle :: rsAddressing :: rsRead :: rsRefill :: Nil = Enum(4)
  val rState = RegInit(rsIdle)
  val rAddr = RegInit(0.U(32.W))
  val rBank = RegInit(0.U(config.bankNumWidth.W))
  val rBuf = Mem(config.lineBankNum, UInt(32.W))
  val rValid = RegInit(0.U(config.lineBankNum.W))
  val rRefillWay = RegInit(0.U(config.wayNumWidth.W))

  // output state
  val osNone :: osKnown1 :: osKnown2 :: osRead :: Nil = Enum(4)
  val oState = RegInit(osNone)
  val oKnownInst1 = RegInit(0.U(32.W))
  val oKnownInst2 = RegInit(0.U(32.W))
  val oReadWay = RegInit(0.U(config.wayNumWidth.W))
  val oReadBank = RegInit(0.U(config.bankNumWidth.W))

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
  io.axiReadAddrOut.arid := 0.U
  io.axiReadAddrOut.araddr := rAddr
  io.axiReadAddrOut.arvalid := rState === rsAddressing
  io.axiReadAddrOut.arlen := 7.U
  io.axiReadAddrOut.arsize := 2.U
  io.axiReadAddrOut.arburst := 2.U
  io.axiReadOut.rready := rState === rsRead

  dataMemAddr := Mux(rState === rsRefill, config.sliceSet(rAddr), iSet)
  for (i <- 0 until config.lineBankNum) {
    wDataMem(i) := rBuf(i)
  }

  dataMem.indices.foreach { i =>
    dataMem(i).io.wEn := i.U === rRefillWay && rState === rsRefill
  }

  lruMem.io.setAddr := iSet
  lruMem.io.visit := 0.U
  lruMem.io.visitValid := false.B

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
  hitWay := hitWays.orR() && rState =/= rsRefill
  val hitWayId = Wire(UInt(config.wayNumWidth.W))
  hitWayId := OHToUInt(hitWays)
  // hit axi direct
  val hitAxiDirect = Wire(Bool())
  hitAxiDirect := config.sliceLineAddr(io.iAddr) === config.sliceLineAddr(rAddr) &&
    config.sliceBank(io.iAddr) === rBank && rState === rsRead && io.axiReadIn.rvalid
  // hit axi buf
  val hitAxiBuf = Wire(Bool())
  hitAxiBuf := config.sliceLineAddr(io.iAddr) === config.sliceLineAddr(rAddr) &&
    rValid(config.sliceBank(io.iAddr)) && (rState === rsRead && rState === rsRefill)
  // invalid addr
  val invalidAddr = Wire(Bool())
  invalidAddr := io.iAddr(1, 0) =/= 0.U

  lruMem.io.visitValid := hitAxiDirect || hitAxiBuf || hitWay

  when (io.enable) {
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
      lruMem.io.visit := rRefillWay
    } .elsewhen (hitAxiBuf) {
      io.inst1Valid := true.B
      io.inst2Valid := iBank =/= 7.U && rValid(iBank + 1.U)
      oState := osKnown2
      oKnownInst1 := rBuf(iBank)
      oKnownInst2 := rBuf(iBank + 1.U)
      lruMem.io.visit := rRefillWay
      lruMem.io.visit := rRefillWay
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
      when (rState === rsIdle) {
        rState := rsAddressing
        rAddr := io.iAddr
        rBank := iBank
        rValid := 0.U
        rRefillWay := lruMem.io.waySel
      }
    }
  }

  // axi read handle
  switch (rState) {
    is (rsAddressing) {
      rState := Mux(io.axiReadAddrIn.arready, rsRead, rsAddressing)
    }
    is (rsRead) {
      rState := Mux(io.axiReadIn.rlast && io.axiReadIn.rvalid, rsRefill, rsRead)
      rBuf(rBank) := io.axiReadIn.rdata
      rValid := Mux(io.axiReadIn.rvalid, setBit(rValid, rBank), rValid)
      rBank := Mux(io.axiReadIn.rvalid, rBank + 1.U, rBank)
    }
    is (rsRefill) {
      rState := rsIdle
      tagMem(fuse(rRefillWay, config.sliceSet(rAddr))) := config.sliceTag(rAddr)
      validMem(rRefillWay) := setBit(validMem(rRefillWay), config.sliceSet(rAddr))
    }
  }
}

object ICache extends App {
  new ChiselStage execute(args, Seq(ChiselGeneratorAnnotation(
    () =>
      new ICache(new CacheConfig(wayNum = 2, setWidth = 8)))))
}
