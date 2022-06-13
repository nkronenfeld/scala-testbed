package ndk

import org.scalatest.funspec.AnyFunSpec

class ModularArithmeticTest extends AnyFunSpec {
  describe("inverse") {
    it("should basically work") {
      assert(ModularArithmetic.inverse(1, 15) === Some(1))
      assert(ModularArithmetic.inverse(2, 15) === Some(8))
      assert(ModularArithmetic.inverse(3, 15) === None)
      assert(ModularArithmetic.inverse(4, 15) === Some(4))
      assert(ModularArithmetic.inverse(5, 15) === None)
      assert(ModularArithmetic.inverse(6, 15) === None)
      assert(ModularArithmetic.inverse(7, 15) === Some(13))
      assert(ModularArithmetic.inverse(8, 15) === Some(2))
      assert(ModularArithmetic.inverse(9, 15) === None)
      assert(ModularArithmetic.inverse(10, 15) === None)
      assert(ModularArithmetic.inverse(11, 15) === Some(11))
      assert(ModularArithmetic.inverse(12, 15) === None)
      assert(ModularArithmetic.inverse(13, 15) === Some(7))
      assert(ModularArithmetic.inverse(14, 15) === Some(14))
    }
    it("should calculate i correctly") {
      assert(11 === ModularArithmetic.i(15))
      assert(51 === ModularArithmetic.i(100))
      assert(1 === ModularArithmetic.i(7))
    }
    it("our two solutions should agree") {
      (3 to 1000).foreach(n => assert(ModularArithmetic.i(n) === ModularArithmetic.i2(n)))
    }
    it("should solve our problem") {
      var sum = 0L
      var lastTime = System.currentTimeMillis()
      (3 to 20000000).foreach { n =>
        sum += ModularArithmetic.i2(n)
        if (0 == (n % 10000)) {
          val time = System.currentTimeMillis()
          println(s"Elapsed time at ${n}: ${(time - lastTime)/1000.0} sec")
          println(s"Sum so far: ${sum}")
        }
      }
      println(s"The answer is ${sum}")
    }
  }
}
