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

  // helper function
  val fuse = (wayId: UInt, setAddr: UInt) =>
    Cat(
      wayId.pad(config.wayNumWidth),
      setAddr.pad(config.setWidth)
    )

  // data mem
  val dataMem = Module(new BlockMem(new MemConfig(
    depth = config.wayNum * config.lineNums,
    lineSize = 8))
  )

  // control state
  val validMem = VecInit(Seq.fill(config.wayNum){ 0.U(config.lineNums.W) })
  val tagMem = Mem(config.wayNum * config.lineNums, UInt(config.tagWidth.W))
  val lruMem = Module(new LruMem(config))

  // axi state
  val rIdle :: rAddressing :: rRead :: rRefill :: Nil = Enum(4)
  val rState = RegInit(rIdle)
  val rAddr = RegInit(0.U(32.W))
  val rBank = RegInit(0.U(3.W))
  val rBuf = Mem(8, UInt(32.W))
  val rValid = RegInit(0.U(8.W))

  // wire defs
  val iTag = Wire(UInt(config.tagWidth.W))
  val iSet = Wire(UInt(config.setWidth.W))
  val iBank = Wire(UInt(3.W))
  iTag := config.sliceTag(io.iAddr)
  iSet := config.sliceSet(io.iAddr)
  iBank := config.sliceBank(io.iAddr)

  // defualt output
  io.inst1 := 0.U
  io.inst2 := 0.U
  io.inst1Valid := false.B
  io.inst2Valid := true.B
  io.axiReadAddrOut.arid := 0.U
  io.axiReadAddrOut.araddr := 0.U
  io.axiReadAddrOut.arvalid := false.B
  io.axiReadAddrOut.arlen := 7.U
  io.axiReadAddrOut.arsize := 2.U
  io.axiReadAddrOut.arburst := 2.U
  io.axiReadOut.rready := false.B
}

object ICache extends App {
  (new ChiselStage)execute(args, Seq(ChiselGeneratorAnnotation(
    () =>
      new ICache(new CacheConfig(wayNum = 2, setWidth = 8)))))
}
