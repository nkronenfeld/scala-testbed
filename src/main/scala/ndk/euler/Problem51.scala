package ndk
package ndk.euler

import scala.collection.mutable

object Problem51 {
  def replacements(p: Int): Seq[Seq[Int]] = {
    val ps = p.toString
    (0 until ps.length)
      .flatMap(n1 => (0 until ps.length).combinations(n1))
      .map(cs =>
        (0 to 9).map(_.toString).map(d =>
          cs.foldLeft(ps)((s, n) => s.take(n) + d + s.drop(n + 1))
        )
          .map(_.toInt)
      )
  }

  def mostDigits (seq: Seq[Int]): Seq[Int] = {
    val digitsByMembers = seq
      .map(n => math.log10(n).floor.toInt + 1)
      .foldLeft(mutable.Map[Int, Int]()) { (accum, d) =>
        if (accum.contains(d)) accum(d) = accum(d) + 1
        else accum(d) = 1
        accum
      }
    val maxD = digitsByMembers.maxBy(_._2)._1

    seq.filter(n => math.log10(n).floor.toInt + 1 == maxD)
  }

  def main (args: Array[String]): Unit = {
    var done = false
    var n = 1
    while (!done) {
      n = n + 1
      val p = Primes.nthPrime(n)
      if (0 == (n % 1000)) {
        println(s"Checking ${n}th prime ${p}")
      }

      val hits = replacements(p)
        .map(_.distinct)
        .map(mostDigits)
        .map(_.filter(Primes.isPrime))
        .filter(_.length >= 8)
      if (hits.nonEmpty) {
        done = true
        println(s"${n}th prime ${p} results in ${hits}")
        hits.map(_.map(p1 => println(s"The factors of ${p1} are ${Primes.factors(p1)}")))
      }
    }
  }
}
