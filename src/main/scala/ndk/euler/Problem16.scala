package ndk
package ndk.euler

object Problem16 {
  def main (args: Array[String]): Unit = {
    (1 to 10).foreach(n => println(pow2nBigInt(n)))
    val startTime = System.nanoTime()
    val n = pow2nBigInt(1000)
    println(s"n: ${n}")
    println(s"digits: ${n.toString.split("").map(_.toInt).sum}")
    val endTime = System.nanoTime()
    println(s"Elapsed time: ${(endTime - startTime) / 1000000.0}ms")
  }

  def pow2nInt (n: Int): Int = (1 to n).map(_ => 2).product

  def pow2nBigInt (n: Int): BigInt = {
    var result = BigInt.int2bigInt(1)
    val maxP2 = (math.log(n)/math.log(2)).floor.toInt

    val n2 = BigInt.int2bigInt(2)
    var nP2 = BigInt.int2bigInt(2)
    (0 to maxP2).foreach { p2 =>
      if (p2 > 0) nP2 = nP2 * nP2
      val a = pow2nInt(p2)
      val b = a & n
      if (0 != (pow2nInt(p2) & n)) {
        result = result * nP2
      }
    }

    result
  }
}
