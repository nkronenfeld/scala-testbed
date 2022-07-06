package ndk
package ndk.euler



import util.Timing



object Problem50 {
  def main (args: Array[String]): Unit = Timing.time {
    var n = 1
    var best = 0
    var bestLen = 0
    val limit = 1000000
    while (Primes.nthPrime(n) < limit) {
      if (0 == (n % 10000)) println(s"Testing ${n}th prime")
      val (value, length) = largestPrimeConsecutivePrimeSum(n, limit)
      if (length > bestLen) {
        best = value
        bestLen = length
        println(s"New best: ${best} starting at ${Primes.nthPrime(n)}, and going on for ${length} primes")
      }
      n += 1
    }
    println(s"Best prime: ${best}")
  }

  def largestPrimeConsecutivePrimeSum (startN: Int, limit: Int): (Int, Int) = {
    var sum = Primes.nthPrime(startN)
    var n = startN
    var best: Int = sum
    var bestLength: Int = 1
    while (sum < limit) {
      if (Primes.isPrime(sum)) {
        best = sum
        bestLength = n - startN + 1
      }
      n += 1
      sum += Primes.nthPrime(n)
    }
    (best, bestLength)
  }
}
