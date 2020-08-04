package dcache

import chisel3.iotesters
import chisel3.iotesters.{PeekPokeTester}

import scala.collection.immutable.List

class LruFsmUnitTester(c: LruFsm) extends PeekPokeTester(c) {
  val wayNum = c.wayNum
  val perms = (0 until wayNum).toList.permutations.toList

  /**
    * Lru FSM Golden model.
    * Compute next state and selection for current state and visit node
    */
  def nextState(current: Int, visit: Int): Int = {
    def nextPerm(p: List[Int], visit: Int): List[Int] = {
      val loc = p.indexOf(visit)
      p.take(loc) ++ p.drop(loc + 1) :+ visit
    }
    val perm2state = (i: List[Int]) => perms.indexOf(i)

    perm2state(nextPerm(perms(current), visit))
  }

  def waySel(current: Int): Int = {
    perms(current).head
  }

  for (current <- perms.indices)
    for (visit <- 0 until wayNum) {
      poke(c.io.current, current)
      poke(c.io.visit, visit)
      expect(c.io.sel, waySel(current))
      expect(c.io.next, nextState(current, visit))
    }
}

