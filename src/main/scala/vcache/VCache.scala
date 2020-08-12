package vcache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import types._

class WriteReq(val lineBankNum: Int) extends Bundle {
  val addr = UInt(32.W)
  val data = Vec(lineBankNum, UInt(32.W))
}

class VCache(val depth: Int = 16, val lineBankNum: Int = 8) extends Module {
  val io = IO(new Bundle {
    // cpu interface
    val wReq = Flipped(Decoupled(new WriteReq(lineBankNum)))
    val rAddr = Input(UInt(32.W))
    val rData = Output(Vec(lineBankNum, UInt(32.W)))
    val rHit = Output(Bool())
    // axi interface
    val axiWrite = new AxiWriteInterface
  })

}

object VCache extends App {
  new ChiselStage execute(args, Seq(ChiselGeneratorAnnotation(
    () =>
      new VCache(depth = 16, lineBankNum = 8))))
}
