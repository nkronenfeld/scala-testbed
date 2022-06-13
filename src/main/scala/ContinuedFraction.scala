package ndk

import scala.collection.mutable

trait ContinuedFraction[N] {
  implicit val integral: Integral[N]
  private val knownTerms = mutable.ArrayBuffer[N]()

  // Get the (0-based) n'th term of the continued fraction
  protected def nextTerm(): N
  protected def initialize (initialTerms: N*): Unit = {
    assert(knownTerms.isEmpty)
    knownTerms.addAll(initialTerms)
  }

  private def extendTo (n: Int): Unit =
    while (knownTerms.length < n) {
      knownTerms.append(nextTerm())
    }

  def convergent (n: Int): Rational[N] = {
    extendTo(n)
    toRational(termsTo(n))
  }

  def termsTo (n: Int): Seq[N] = {
    extendTo(n)
    knownTerms.iterator.take(n).toSeq
  }

  protected def toRational (terms: Seq[N]): Rational[N] = {
    import integral.mkNumericOps
    terms.foldRight(Rational(integral.one, integral.zero)) { (next, accum) =>
      Rational(next * accum.numerator + accum.denominator, accum.numerator).reduced
    }
  }

  protected def allKnownTerms: Seq[N] = knownTerms.toSeq
}

class IteratorBasedContinuedFraction [N] (seq: Iterator[N])(implicit val integral: Integral[N]) extends ContinuedFraction [N] {
  override protected def nextTerm(): N = seq.next()
}

class SquareRootContinuedFraction[N] (n: N)(implicit val integral: Integral[N]) extends ContinuedFraction[N] {
  private val ratN = Rational[N](n, integral.one)(integral)

  // Initialize with the base sqrt
  {
    import integral.mkNumericOps
    import integral.mkOrderingOps
    var firstOrderApprox = integral.fromInt(math.sqrt(integral.toDouble(n)).floor.toInt)
    assert(firstOrderApprox * firstOrderApprox < n)
    while ((firstOrderApprox + integral.one) * (firstOrderApprox + integral.one) < n) {
      firstOrderApprox = firstOrderApprox + integral.one
    }
    initialize(firstOrderApprox)
  }

  override protected def nextTerm(): N = {
    import integral.mkNumericOps

    val soFar = allKnownTerms
    var r = toRational(soFar :+ integral.one)
    // see if we're below or above
    (r * r).compareTo(ratN) match {
      case 0 => integral.zero
      case -1 =>
        // Less than... add numbers until square > n
        var curApprox = integral.one
        var nextApprox = curApprox + integral.one
        r = toRational(soFar :+ nextApprox)
        while ((r * r).compareTo(ratN) < 0) {
          curApprox = nextApprox
          nextApprox = curApprox + integral.one
          r = toRational(soFar :+ nextApprox)
        }
        curApprox

      case 1 =>
        // Greater than... add numbers until square < n
        var curApprox = integral.one
        var nextApprox = curApprox + integral.one
        r = toRational(soFar :+ nextApprox)
        while ((r * r).compareTo(ratN) > 0) {
          curApprox = nextApprox
          nextApprox = curApprox + integral.one
          r = toRational(soFar :+ nextApprox)
        }
        curApprox
    }
  }
}

object ContinuedFraction {
  def main (args: Array[String]): Unit = {
    val d = 3
    val sqrt = new SquareRootContinuedFraction[BigInt](d)
    println(sqrt.termsTo(100))
    (1 to 100).iterator.map(n => (n, sqrt.convergent(n))).foreach { case (n, c) =>
      val cc = c * c
      val dd = c.numerator * c.numerator - d * c.denominator * c.denominator
      println(n, c, cc, cc.numerator.toDouble / cc.denominator.toDouble, dd)
    }
  }
//  def continuedFractionOf (n: BigDecimal): ContinuedFraction = new ContinuedFraction(new Iterator[Long] {
//    var pVal = n
//    override def hasNext: Boolean = true
//    override def next(): Long = {
//      val result = pVal.toLong
//      pVal = 1.0 / (pVal - result)
//      result
//    }
//  })
}
