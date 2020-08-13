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

  val depthWidth = log2Ceil(depth)
  val bankNumWidth = log2Ceil(lineBankNum)
  val io = IO(new Bundle {
    // cpu interface
    val wReq = Flipped(Decoupled(new WriteReq(lineBankNum)))
    val rEn = Input(Bool())
    val rAddr = Input(UInt(32.W))
    val rData = Output(Vec(lineBankNum, UInt(32.W)))
    val rHit = Output(Bool())
    // axi interface
    val axiWrite = new AxiWriteInterface
  })

  // fifo state
  val itemAddr = RegInit(VecInit.tabulate(depth) { _ => 0.U(32.W) })
  val itemData = RegInit(VecInit.tabulate(depth) { _ => VecInit.tabulate(lineBankNum) { _ => 0.U(32.W) } })
  val itemValid = RegInit(0.U(depth.W))
  val pEnq = RegInit(0.U(depthWidth))
  val pDeq = RegInit(0.U(depthWidth))
  val qFull = RegInit(false.B)

  // axi write state
  val wsIdle :: wsAddressing :: wsWrite :: wsResp :: Nil = Enum(4)
  val wState = RegInit(wsIdle)
  val wBank = RegInit(0.U(bankNumWidth.W))

  // input buffering
  val rAddrBuf = RegInit(0.U(32.W))
  rAddrBuf := Mux(io.rEn, io.rAddr, rAddrBuf)

  // axi output
  io.axiWrite.awid := 0.U
  io.axiWrite.awsize := 2.U
  io.axiWrite.awlen := (lineBankNum - 1).U
  io.axiWrite.awburst := 2.U
  io.axiWrite.awaddr := itemAddr(pDeq)
  io.axiWrite.awvalid := wState === wsAddressing
  io.axiWrite.wid := 0.U
  io.axiWrite.wdata := itemData(pDeq)(wBank)
  io.axiWrite.wstrb := "hf".U
  io.axiWrite.wvalid := wState === wsWrite
  io.axiWrite.wlast := wBank === (lineBankNum - 1).U
  io.axiWrite.bready := wState === wsResp

  // hit handle
  val hitItems = Wire(UInt(depth.W))
  hitItems := VecInit.tabulate(depth) { i =>
    itemAddr(i) === rAddrBuf && itemValid(i)
  }.asUInt
  val hitItemId = Wire(UInt(depthWidth.W))
  hitItemId := OHToUInt(hitItems)
  io.rHit := hitItems.orR
  io.rData := itemData(hitItemId)

  // duplicate check
  val dupItems = Wire(UInt(depth.W))
  dupItems := VecInit.tabulate(depth) { i =>
    itemAddr(i) === io.wReq.bits.addr && itemValid(i)
  }.asUInt
  val dupItem = Wire(Bool())
  dupItem := dupItems.orR
  val dupItemId = Wire(UInt(depthWidth.W))
  dupItemId := OHToUInt(dupItems)

  val nextFull = Wire(Bool())
  qFull := nextFull
  nextFull := qFull // default

  // write request handle
  io.wReq.ready := !qFull
  when (io.wReq.valid && !qFull) {
    pEnq := pEnq + 1.U
    itemAddr(pEnq) := io.wReq.bits.addr
    itemData(pEnq) := io.wReq.bits.data
    itemValid := setBit(itemValid, pEnq)

    when (dupItem) {
      itemValid := clearBit(itemValid, dupItemId)
    }

    when (pEnq + 1.U === pDeq) {
      nextFull := true.B
    }
  }

  val busy = Wire(Bool())
  busy := qFull || pEnq =/= pDeq

  // axi handle
  switch (wState) {
    is (wsIdle) {
      wState := Mux(busy, wsAddressing, wsIdle);
    }
    is (wsAddressing) {
      wState := Mux(io.axiWrite.awready, wsWrite, wsAddressing);
      wBank := 0.U
    }
    is (wsWrite) {
      wBank := Mux(io.axiWrite.wready, wBank + 1.U, wBank)
      wState := Mux(io.axiWrite.wlast, wsResp, wsWrite);
    }
    is (wsResp) {
      when (io.axiWrite.bvalid) {
        wState := wsIdle
        nextFull := false.B
        pDeq := pDeq + 1.U
      } .otherwise {
        wState := wsResp
      }
    }
  }
}

object VCache extends App {
  new ChiselStage execute(args, Seq(ChiselGeneratorAnnotation(
    () =>
      new VCache(depth = 16, lineBankNum = 8))))
}
