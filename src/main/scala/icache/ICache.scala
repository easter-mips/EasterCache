package icache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import lru.LruMem
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
  val fuse = (wayId: UInt, setAddr: UInt) => Cat(wayId, setAddr)

  val bankMem = SyncReadMem(config.wayNum * config.lineNums, Vec(8, UInt(32.W)))

  // defualt output
  io.inst1 := bankMem.read(0.U(9), true.B)(0)
  io.inst2 := bankMem.read(1.U(9), true.B)(1)
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
