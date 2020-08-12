package mem

import chisel3._

class bank_ram(val setWidth: Int) extends BlackBox {
  val io = IO(new Bundle {
    val clka = Input(Clock())
    val ena = Input(Bool())
    val wea = Input(Bool())
    val addra = Input(UInt(setWidth.W))
    val dina = Input(UInt(32.W))
    val douta = Output(UInt(32.W))
  })
}
