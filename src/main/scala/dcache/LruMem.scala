package dcache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import scala.collection.immutable.List

class LruMem(val config: CacheConfig) extends Module {
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
  val lruWidth = getLruWidth(config.wayNum)
  val io = IO(new Bundle {
    val setAddr = Input(UInt(config.setWidth.W))
    val visit = Input(UInt(config.wayNumWidth.W))
    val visitValid = Input(Bool())
    val waySel = Output(UInt(config.wayNumWidth.W))
  })

  val lruMem = Mem(config.lineNums, UInt(lruWidth.W))
  val validMem = RegInit(0.U(config.lineNums.W))

  def readLru(addr: UInt): UInt = Mux(validMem(addr), lruMem(addr), 0.U(lruWidth.W))

  val lruFsm = Module(new LruFsm(config.wayNum))
  lruFsm.io.current := readLru(io.setAddr)
  lruFsm.io.visit := io.visit
  io.waySel := lruFsm.io.sel

  when (io.visitValid) {
    validMem := setBit(validMem, io.setAddr)
    lruMem(io.setAddr) := lruFsm.io.next
  }
}

object LruMem extends App {
  (new ChiselStage)execute(args, Seq(ChiselGeneratorAnnotation(
    () =>
      new LruMem(new CacheConfig(wayNum = 2, setWidth = 8)))))
}
