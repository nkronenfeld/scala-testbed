package ndk
package ndk.euler

object Problem86 {
  // given sides a|b|c, assuming one travels first on face a|b, the full length of b, but partially up a at
  // point x, then on a|c from that point to the opposite corner, the distance travelled is:
  //
  //   dist(a|b|c) = sqrt(x^2 + b^2) + sqrt((a-x)^2 + c^2)
  //
  // to find the minimum:
  //
  //   d(dist(a|b|c))/dx =   (x^2 + b^2)^-1/2 * (2x) + ((a-x)^2 + c^2)^-1/2 * (-2a + 2 x)
  //
  // setting that equal to 0 we get
  //
  //   x = (2 a b^2 +/- sqrt(4 a^2 b^4 - 4 a^2 b^2 (b^2 - c^2)))/2(b^2 - c^2)
  private def getX (a: Int, b: Int, c: Int): Seq[Double] = {
    val rootDiscriminant = math.sqrt(4 * a * a * b * b * b * b - 4 * a * a * b * b * (b * b - c * c))
    val candidate1 = (2 * a * b * b + rootDiscriminant) / (2 * (b * b - c * c))
    val candidate2 = (2 * a * b * b - rootDiscriminant) / (2 * (b * b - c * c))
    Seq(candidate1, candidate2).filter(x => 0 <= x && x <= a )
  }

  private def allOrders[T] (s: T*): Seq[Seq[T]] = {
    if (s.length == 1) Seq(s)
    else s.zipWithIndex.flatMap { case (item, index) =>
      val rest: Seq[T] = s.slice(0, index) ++ s.slice(index+1, s.length)
      allOrders(rest:_*).map ( sPrime => Seq(item) ++ sPrime )
    }
  }

  private def distance (a: Int, b: Int, c: Int, x: Double): Double =
    math.sqrt(x * x + b * b) + math.sqrt((a - x) * (a - x) + c * c)

  private def getBestDistance (a: Int, b: Int, c: Int): Option[Double] = {
    val distances = allOrders(a, b, c)
      .flatMap(ordering => getX(ordering(0), ordering(1), ordering(2)).map(x =>
        distance(ordering(0), ordering(1), ordering(2), x)
      ))
    if (distances.isEmpty) None
    else Some(distances.min)
  }
  private def getDistances (a: Int, b: Int, c: Int): Seq[Double] = {
    val distances = allOrders(a, b, c)
      .flatMap(ordering => getX(ordering(0), ordering(1), ordering(2)).map(x =>
        distance(ordering(0), ordering(1), ordering(2), x)
      ))
    distances
  }

  private def countLeastIntegerCuboids (limit: Int): Int = {
    val candidates: Seq[Seq[Int]] =
      (1 to limit).flatMap { a =>
        (1 to limit).flatMap { b =>
          (1 to limit).flatMap { c =>
            getBestDistance(a, b, c).toSeq.flatMap { distance =>
              if ((distance - distance.round).abs < 1E-8) {
                Seq(Seq(a, b, c))
              } else {
                Seq[Seq[Int]]()
              }
            }
          }
        }
      }.map(_.sorted)
        .distinct

    candidates.map(s => (s(0), s(1), s(2))).sorted.foreach { s =>
      println(s"${s}: ${getBestDistance(s._1, s._2, s._3)}")
    }
    println(s"There are ${candidates.count(_.toSet.size == 2)} with two identical sides")
    println(s"There are ${candidates.count(_.toSet.size == 1)} with three identical sides")

    candidates.size
  }

  private def countIntegerCuboids (limit: Int): Int = {
    val candidates: Seq[Seq[Int]] =
      (1 to limit).flatMap { a =>
        (1 to limit).flatMap { b =>
          (1 to limit).flatMap { c =>
            getDistances(a, b, c)
              .filter(distance => (distance - distance.round).abs < 1E-8)
              .reduceOption(_ min _)
              .map (_ => Seq(a, b, c).sorted.reverse)
          }
        }
      }.distinct

    candidates.map(s => (s(0), s(1), s(2))).sorted.foreach { s =>
      println(s"${s}: ${getDistances(s._1, s._2, s._3).filter(distance => (distance - distance.round).abs < 1E-8).min}, ${getBestDistance(s._1, s._2, s._3)}")
    }

    candidates.size
  }

  def main (args: Array[String]): Unit = {
    println(countLeastIntegerCuboids(99))
  }
}
