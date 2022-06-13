package ndk
package ndk.euler

object Problem66 {
  def main (args: Array[String]): Unit = {
    val startTime = System.nanoTime()

    val (maxD, maxX, maxY) =
      (2 to 1000)
        .filter { n =>
          val n_root = math.sqrt(n).toInt
          n != n_root * n_root
        }.map { d =>
        val (x, y) = solve(d)
        (d, x, y)
      }.maxBy(_._2)

    val endTime = System.nanoTime()
    println(s"Maximum at ${maxX}^2 - ${maxD} ${maxY}^2 = 1")
    println(s"Solved in ${(endTime - startTime)/1000000.0}ms")
  }

  def solve (d: Int): (BigInt, BigInt) = {
    val sqrtD = new SquareRootContinuedFraction[BigInt](d)
    var result: Option[(BigInt, BigInt)] = Option.empty
    var n = 1
    while (result.isEmpty) {
      val r = sqrtD.convergent(n)
      if (1 == r.numerator * r.numerator - d * r.denominator * r.denominator) {
        result = Some(r.numerator, r.denominator)
      } else {
        n += 1
      }
    }
    result.get
  }
}
