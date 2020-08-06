package mem

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import types._

class BlockMem(val config: MemConfig) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(config.addrWidth.W))
    val wEn = Input(Bool())
    val dataIn = Input(Vec(config.lineSize, UInt(config.bankWidth.W)))
    val dataOut = Output(Vec(config.lineSize, UInt(config.bankWidth.W)))
  })

  val mem = SyncReadMem(config.depth, Vec(config.lineSize, UInt(config.bankWidth.W)))
  // read
  io.dataOut := mem.read(io.addr, true.B)
  // write
  when (io.wEn) {
    mem.write(io.addr, io.dataIn)
  }
}

object BlockMem extends App {
  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(
    () =>
      new BlockMem(new MemConfig(depth = 256, lineSize = 8)))))
}
