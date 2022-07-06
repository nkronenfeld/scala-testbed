package ndk
package ndk.euler

object Problem21 {
  def main (args: Array[String]): Unit = {
    val startTime = System.nanoTime()
    var sum = 0
    (2 to 10000).foreach { n =>
      val dn = properDivisorSum(n)
      if (dn > n && properDivisorSum(dn) == n) {
        println(s"${n} and ${dn} are paired")
        sum += n + dn
      }
    }
    println(s"The sum of all amicable numbers up to 10000 is ${sum}")
    val endTime = System.nanoTime()

    println(s"Elapsed time: ${(endTime - startTime) / 1000000.0}ms")
  }

  def properDivisorSum (n: Int): Int = {
    val primeFactors = Primes.factors(n)
    val properFactors = 1 +: (1 until primeFactors.length).flatMap(n => primeFactors.combinations(n).map(_.product))
    properFactors.sum
  }
}
