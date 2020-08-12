package mem

import chisel3._
import chisel3.util._
import chisel3.experimental._

class bank_mem(val setWidth: Int) extends BlackBox {
  val io = IO(new Bundle {
    val clka = Input(Clock())
    val ena = Input(Bool())
    val wea = Input(Bool())
    val addra = Input(UInt(setWidth.W))
    val dina = Input(UInt(32.W))
    val douta = Output(UInt(32.W))
  })
}
