package types

import chisel3._
import chisel3.util._

import scala.collection.immutable.List

class AxiReadInterface extends Bundle {
  // address channel
  val arid = Output(UInt(4.W))
  val araddr = Output(UInt(32.W))
  val arvalid = Output(Bool())
  val arlen = Output(UInt(4.W))
  val arsize = Output(UInt(3.W))
  val arburst = Output(UInt(2.W))
  val arready = Input(Bool())

  // read channel
  val rready = Output(Bool())
  val rid = Input(UInt(4.W))
  val rdata = Input(UInt(32.W))
  val rresp = Input(UInt(2.W))
  val rlast = Input(Bool())
  val rvalid = Input(Bool())
}

class AxiWriteInterface extends Bundle {
  // address channel
  val awid = Output(UInt(4.W))
  val awaddr = Output(UInt(32.W))
  val awlen = Output(UInt(4.W))
  val awsize = Output(UInt(3.W))
  val awburst = Output(UInt(2.W))
  val awvalid = Output(Bool())
  val awready = Input(Bool())

  // write channel
  val wid = Output(UInt(4.W))
  val wdata = Output(UInt(32.W))
  val wstrb = Output(UInt(4.W))
  val wlast = Output(Bool())
  val wvalid = Output(Bool())
  val wready = Input(Bool())

  // response channel
  val bready = Output(Bool())
  val bid = Input(UInt(4.W))
  val bresp = Input(UInt(8.W))
  val bvalid = Input(Bool())
}

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

class HitStats extends Bundle {
  val hitCount = UInt(32.W)
  val missCount = UInt(32.W)
}

class CacheConfig(val wayNum: Int, val setWidth: Int, val lineBankNum: Int = 8, val transNum: Int = 2) {
  val bankNumWidth = log2Ceil(lineBankNum)
  val tagWidth = 32 - 2 - bankNumWidth - setWidth
  val wayNumWidth = log2Ceil(wayNum)
  val lineAddrWidth = 32 - 2 - bankNumWidth
  val lineNums = List.fill(setWidth)(2).foldLeft(1)(_ * _)

  def sliceTag(x: UInt): UInt = x(31, 32 - tagWidth)
  def sliceSet(x: UInt): UInt = x(31 - tagWidth, 32 - tagWidth - setWidth)
  def sliceBank(x: UInt): UInt = x(1 + bankNumWidth, 2)
  def sliceLineAddr(x: UInt): UInt = x(31, bankNumWidth + 2)
}

class CacheSettings(val icacheConfig: CacheConfig, val dcacheConfig: CacheConfig, val vcacheDepth: Int) {
}

class MemConfig(val depth: Int, val lineSize: Int) {
  val addrWidth = log2Ceil(depth)
  val bankWidth = 32
}
