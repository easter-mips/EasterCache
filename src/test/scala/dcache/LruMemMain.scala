package dcache

import chisel3._

object LruMemMain extends App {
  println("LRU FSM tests begin")
  val configs = Seq((4, 7), (2, 8)).map { (w) => new CacheConfig(wayNum = w._1, setWidth = w._2) }
  for (config <- configs) {
    iotesters.Driver.execute(args, () => new LruMem(config)) {
      c => new LruMemUnitTester(c)
    }
  }
  println("LRU FSM tests PASS")
}
