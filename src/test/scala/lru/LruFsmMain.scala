package lru

import chisel3.iotesters

object LruFsmMain extends App {
  println("LRU FSM tests begin")
  for (i <- 2 to 4 by 2) {
    println(s"Testing LRU FSM for $i way(s)")
    iotesters.Driver.execute(args, () => new LruFsm(wayNum = i)) {
      c => new LruFsmUnitTester(c)
    }
  }
  println("LRU FSM tests PASS")
}
