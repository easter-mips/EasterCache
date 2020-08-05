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

class CacheConfig(val wayNum: Int, val setWidth: Int) {
  val tagWidth = 32 - 5 - setWidth
  val wayNumWidth = log2Ceil(wayNum)
  val lineAddrWidth = 32 - 5
  val lineNums = List.fill(setWidth)(2).foldLeft(1)(_ * _)

  def sliceTag(x: UInt): UInt = x(31, 32 - tagWidth)
  def sliceSet(x: UInt): UInt = x(31 - tagWidth, 32 - tagWidth - setWidth)
  def sliceBank(x: UInt): UInt = x(4, 2)
  def sliceLineAddr(x: UInt): UInt = x(31, 5)
}
