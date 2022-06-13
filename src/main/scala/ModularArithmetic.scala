package ndk

object ModularArithmetic {
  def inverse(n: Int, modulus: Int): Option[Int] = Range(1, modulus).find(a => 1 == (a * n) % modulus)

  // i and i2 are for problem 451
  def i(modulus: Int): Int = (modulus - 2 to 1 by -1).find(n => inverse(n, modulus).exists(_ == n)).get // 1 should always work

  def i2(modulus: Int): Int = (modulus - 3 to 1 by -1).map(_ * modulus + 1).find(OldPrimes.isSquare).map(n => math.sqrt(n).toInt).getOrElse(1)
}
