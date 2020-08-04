package dcache

import chisel3.iotesters
import chisel3.iotesters.{PeekPokeTester}

class LruMemUnitTester(c: LruMem) extends PeekPokeTester(c) {
  /**
    * LruMem Golden Model
    */
  class LruMemModel {
    var state: List[Int] = (0 until c.config.wayNum).toList
    def sel = state.head
    def update(visit: Int) = {
      def nextState(perm: List[Int], visit: Int): List[Int] = {
        val loc = perm.indexOf(visit)
        perm.take(loc) ++ perm.drop(loc + 1) :+ visit
      }

      state = nextState(state, visit)
      sel
    }
  }
  // test initialization
  poke(c.io.visit, 0)
  poke(c.io.visitValid, 0)
  for (set <- 0 until c.config.lineNums) {
    poke(c.io.setAddr, set)
    expect(c.io.waySel, 0)
  }
  // test next
  poke(c.io.visitValid, 1)
  for (set <- 0 until c.config.lineNums) {
    var model = new LruMemModel
    poke(c.io.setAddr, set)
    for (i <- 0 until 10) {
      val visit = rnd.nextInt(c.config.wayNum - 1)
      poke(c.io.visit, visit)
      step(1)
      expect(c.io.waySel, model.update(visit))
    }
  }
}
