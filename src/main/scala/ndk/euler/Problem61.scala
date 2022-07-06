package ndk
package ndk.euler



import util.PositiveIntegerIterator
import util.Timing



object Problem61 {
  def main (args: Array[String]): Unit = Timing.time {
    val n3 = fourDigitVariants(triangleNumber).toSet
    val n4 = fourDigitVariants(squareNumber).toSet
    val n5 = fourDigitVariants(pentagonalNumber).toSet
    val n6 = fourDigitVariants(hexagonalNumber).toSet
    val n7 = fourDigitVariants(heptagonalNumber).toSet
    val n8 = fourDigitVariants(octagonalNumber).toSet

    val n4To8 = Seq(n4, n5, n6, n7, n8)

    def numOut (n: Int): String = {
      val types =
        (if (n3.contains(n)) Seq("3") else Seq()) ++
          (if (n4.contains(n)) Seq("4") else Seq()) ++
          (if (n5.contains(n)) Seq("5") else Seq()) ++
          (if (n6.contains(n)) Seq("6") else Seq()) ++
          (if (n7.contains(n)) Seq("7") else Seq()) ++
          (if (n8.contains(n)) Seq("8") else Seq())
      types.mkString(s"${n} [", ",", "]")
    }
    n3.foreach { nn3 =>
      attempt(n4To8, Seq(nn3)).foreach(seq => println(seq.map(numOut), seq.sum))
    }
  }

  def attempt (setsLeft: Seq[Set[Int]], soFar: Seq[Int]): Option[Seq[Int]] = {
    if (setsLeft.isEmpty) {
      if ((soFar.last % 100) == soFar.head / 100) {
        Some(soFar)
      } else {
        None
      }
    } else {
      val target = soFar.last % 100
      val candidates = setsLeft.map { set => set.filter(n => n / 100 == target) }
      candidates.zipWithIndex.foreach { case (set, index) =>
        val nextSetsLeft = setsLeft.slice(0, index) ++ setsLeft.slice(index + 1, setsLeft.length)
        set.foreach { nextCandidate =>
          val result = attempt(nextSetsLeft, soFar :+ nextCandidate)
          if (result.isDefined) return result
        }
      }
      None
    }
  }

  def triangleNumber (n: Int)  = n * (n + 1) / 2
  def squareNumber (n: Int) = n * n
  def pentagonalNumber (n: Int) = n * (3 * n  - 1) / 2
  def hexagonalNumber (n: Int) = n * (2 * n - 1)
  def heptagonalNumber (n: Int) = n * (5 * n - 3) / 2
  def octagonalNumber (n: Int) = n * (3 * n - 2)


  def fourDigitVariants (fcn: Int => Int): Seq[Int] =
    new PositiveIntegerIterator().map(fcn).takeWhile(_ < 10000).filter(_ > 999).toSeq
}
