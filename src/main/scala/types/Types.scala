package types

import chisel3._
import chisel3.util._

import scala.collection.immutable.List

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

class CacheConfig(val wayNum: Int, val setWidth: Int, val lineBankNum: Int = 8) {
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

class MemConfig(val depth: Int, val lineSize: Int) {
  val addrWidth = log2Ceil(depth)
  val bankWidth = 32
}
