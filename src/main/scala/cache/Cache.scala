package cache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import types._
import dcache.DCache
import icache.ICache
import vcache.VCache

class Cache(val settings: CacheSettings) extends Module {
  val io = IO(new Bundle {
    val cacheInst = Input(UInt(5.W))
    // icache signals
    val iEn = Input(Bool())
    val iAddr = Input(UInt(32.W))
    val inst1 = Output(UInt(32.W))
    val inst2 = Output(UInt(32.W))
    val inst1Valid = Output(Bool())
    val inst2Valid = Output(Bool())
    // dcache signals
    val dEn = Input(Bool())
    val dAddr = Input(UInt(32.W))
    val rSize = Input(UInt(3.W))
    val wSize = Input(UInt(3.W))
    val wData = Input(UInt(32.W))
    val rwEn = Input(Bool())
    val wStrb = Input(UInt(4.W))
    val dWait = Output(Bool())
    val rData = Output(UInt(32.W))

    // axi interface
    val instRead = new AxiReadInterface
    val instWrite = new AxiWriteInterface
    val dataRead = new AxiReadInterface
    val dataWrite = new AxiWriteInterface
  })

  // module instances
  val icache = Module(new ICache(settings.icacheConfig))
  val dcache = Module(new DCache(settings.dcacheConfig))
  val vcache = Module(new VCache(depth = settings.vcacheDepth, settings.dcacheConfig.lineBankNum))

  val instAction = Wire(UInt(4.W))
  val dataAction = Wire(UInt(4.W))
  instAction := Mux(io.cacheInst(4), 0.U, io.cacheInst(3, 0))
  dataAction := Mux(io.cacheInst(4), io.cacheInst(3, 0), 0.U)

  // icache interface
  // cpu
  icache.io.enable := io.iEn
  icache.io.iAddr := io.iAddr
  io.inst1 := icache.io.inst1
  io.inst2 := icache.io.inst2
  io.inst1Valid := icache.io.inst1Valid
  io.inst2Valid := icache.io.inst2Valid
  icache.io.action := instAction
  // axi write
  io.instWrite.awid := 0.U
  io.instWrite.awaddr := 0.U
  io.instWrite.awburst := 0.U
  io.instWrite.awlen := 0.U
  io.instWrite.awsize := 0.U
  io.instWrite.awvalid := 0.U
  io.instWrite.wid := 0.U
  io.instWrite.wdata := 0.U
  io.instWrite.wvalid := 0.U
  io.instWrite.wlast := 0.U
  io.instWrite.wstrb := 0.U
  io.instWrite.bready := 0.U
  // axi read
  io.instRead <> icache.io.axiRead

  // dcache interface
  // cpu
  dcache.io.enable := io.dEn
  dcache.io.dAddr := io.dAddr
  dcache.io.dSize := Mux(io.rwEn, io.rSize, io.wSize)
  dcache.io.wData := io.wData
  dcache.io.wEn := !io.rwEn
  dcache.io.wStrb := io.wStrb
  io.dWait := dcache.io.dWait
  io.rData := dcache.io.rData
  dcache.io.action := dataAction
  // axi read
  io.dataRead <> dcache.io.axiRead
  // vcache
  vcache.io.wReq.valid := dcache.io.vcWValid
  vcache.io.wReq.bits.addr := Cat(dcache.io.vcWAddr, 0.U((settings.dcacheConfig.bankNumWidth + 2).W))
  vcache.io.wReq.bits.data := dcache.io.vcWData
  dcache.io.vcReady := vcache.io.wReq.ready
  vcache.io.rEn := dcache.io.vcREn
  vcache.io.rAddr := Cat(dcache.io.vcRAddr, 0.U((settings.dcacheConfig.bankNumWidth + 2).W))
  dcache.io.vcRData := vcache.io.rData
  dcache.io.vcHit := vcache.io.rHit
  // axi write
  io.dataWrite <> vcache.io.axiWrite
}

object Cache extends App {
  new ChiselStage execute(args, Seq(ChiselGeneratorAnnotation(
    () =>
      new Cache(new CacheSettings(
        new CacheConfig(wayNum = 2, setWidth = 7), new CacheConfig(wayNum = 2, setWidth = 7), vcacheDepth = 16
      )))))
}
