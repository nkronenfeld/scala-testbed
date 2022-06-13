package ndk
package ndk.euler

object Problem12 {
  def main (args: Array[String]): Unit = {
    var n = 0
    var t = 0
    var done = false

    while (!done) {
      n = n + 1
      t = t + n
      val factors = Primes.factors(t)
      val divisors = (1 to factors.length).flatMap(n => factors.combinations(n))
      println(s"${n}th triangle number ${t} has ${divisors.length + 1} factors")
      done = divisors.length >= 499
    }
  }

}
