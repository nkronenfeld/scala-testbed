package ndk

import scala.annotation.tailrec

case class Rational [N: Integral] (numerator: N, denominator: N) extends Comparable[Rational[N]] {
  import Rational._
  private val integral = implicitly[Integral[N]]
  private lazy val doubleNegativeOne = Rational(integral.negate(integral.one), integral.negate(integral.one))
  import integral.mkNumericOps

  def reduced: Rational[N] = {
    val gcd = greatestCommonDivisor(numerator, denominator)
    Rational(numerator / gcd, denominator / gcd)
  }
  def inverse: Rational[N] = Rational(denominator, numerator)

  def + (that: Rational[N]): Rational[N] =
    Rational(this.numerator * that.denominator + that.numerator * this.denominator, this.denominator * that.denominator)
      .reduced

  def - (that: Rational[N]): Rational[N] =
    Rational(this.numerator * that.denominator - that.numerator * this.denominator, this.denominator * that.denominator)
      .reduced

  def * (that: Rational[N]): Rational[N] =
    Rational(this.numerator * that.numerator, this.denominator * that.denominator)
      .reduced

  def / (that: Rational[N]): Rational[N] =
    Rational(this.numerator * that.denominator, this.denominator * that.numerator)
      .reduced

  def unary_- : Rational[N] = Rational(-this.numerator, this.denominator)

  override def compareTo(that: Rational[N]): Int = {
    if (integral.lt(this.denominator, integral.zero)) (this * doubleNegativeOne).compareTo(that)
    else if (integral.lt(that.denominator, integral.zero)) this.compareTo(that * doubleNegativeOne)
    else {
      val cx = integral.times(this.numerator, that.denominator)
      val cy = integral.times(that.numerator, this.denominator)
      if (integral.lt(cx, cy)) -1
      else if (integral.gt(cx, cy)) 1
      else 0
    }
  }
}
object Rational {
  class FractionalRational[N: Integral] extends Fractional[Rational[N]] {
    private val integral = implicitly[Integral[N]]

    override def div(x: Rational[N], y: Rational[N]): Rational[N] = x / y
    override def plus(x: Rational[N], y: Rational[N]): Rational[N] = x + y
    override def minus(x: Rational[N], y: Rational[N]): Rational[N] = x - y
    override def times(x: Rational[N], y: Rational[N]): Rational[N] = x * y
    override def negate(x: Rational[N]): Rational[N] = -x
    override def fromInt(x: Int): Rational[N] = Rational(integral.fromInt(x), integral.one)

    override def parseString(str: String): Option[Rational[N]] = {
      val parts = str.split("/").map(_.trim).map(integral.parseString)
      if (parts.exists(_.isEmpty) || parts.length < 1 || parts.length > 2) {
        None
      } else Some(
        if (parts.length == 1) Rational(parts(0).get, integral.one)
        else Rational(parts(0).get, parts(1).get)
      )
    }

    override def toInt(x: Rational[N]): Int = integral.toInt(integral.quot(x.numerator, x.denominator))
    override def toLong(x: Rational[N]): Long = integral.toLong(integral.quot(x.numerator, x.denominator))
    override def toFloat(x: Rational[N]): Float = integral.toFloat(x.numerator) / integral.toFloat(x.denominator)
    override def toDouble(x: Rational[N]): Double = integral.toDouble(x.numerator) / integral.toDouble(x.denominator)

    override def compare(x: Rational[N], y: Rational[N]): Int = x.compareTo(y)
  }

  @tailrec
  def greatestCommonDivisor[N: Integral](a: N, b: N): N = {
    val integral = implicitly[Integral[N]]
    if (integral.lt(a, integral.zero)) greatestCommonDivisor(integral.negate(a), b)
    else if (integral.lt(b, integral.zero)) greatestCommonDivisor(a, integral.negate(b))
    else if (integral.gt(a, b)) greatestCommonDivisor(b, a)
    else if (integral.zero == a) b
    else {
      val q = integral.quot(b, a)
      greatestCommonDivisor(a, integral.minus(b, integral.times(a, q)))
    }
  }

  def main (args: Array[String]): Unit = {
    println(Rational(3, 4) + Rational(2, 3))
  }
}

