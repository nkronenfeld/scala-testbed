package ndk
package ndk.euler.problem460

import scala.collection.mutable

// Problem 460
object Ant {
  def distance (p0: (Int, Int), p1: (Int, Int)): Double = {
    val dx = p1._1 - p0._1
    val dy = p1._2 - p0._2
    math.sqrt(dx * dx + dy * dy)
  }
  def velocity (p0: (Int, Int), p1: (Int, Int)): Double = {
    if (p0._2 == p1._2) p0._2
    else (p1._2 - p0._2) / (math.log(p1._2) - math.log(p0._2))
  }

  def time (p0: (Int, Int), p1: (Int, Int)): Double = distance(p0, p1) / velocity(p0, p1)
  def time (endpoints: ((Int, Int), (Int, Int))): Double = time(endpoints._1, endpoints._2)

  def add (p0: (Int, Int), p1: (Int, Int)): (Int, Int) = (p0._1 + p1._1, p0._2 + p1._2)

  def pathTime (ps: (Int, Int) *): Double =
    (ps.dropRight(1) zip ps.drop(1)).map(time).sum

  def pathSearch (soFar: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    val inputTime = pathTime(soFar:_*)
    // Insert a pair of points, 1 up and in from the centers
    val halfSize = soFar.length / 2

    val frontHalf = soFar.take(halfSize)
    val backHalf = soFar.drop(halfSize)

    var p0 = frontHalf.last
    var p1 = backHalf.head

    var bestTime = inputTime
    var nextTime: Option[Double] = None

    val increments = Seq(
      ((1, 0), (-1, 0)),
      ((1, 1), (-1, 1)),
      ((0, 1), (0, 1)),
      ((2, 0), (-2, 0)),
      ((2, 1), (-2, 1)),
      ((2, 2), (-2, 2)),
      ((1, 2), (-1, 2)),
      ((0, 2), (0, 2)),
      ((3, 0), (-3, 0)),
      ((3, 1), (-3, 1)),
      ((3, 2), (-3, 2)),
      ((3, 3), (-3, 3)),
      ((2, 3), (-2, 3)),
      ((1, 3), (-1, 3)),
      ((0, 3), (0, 3))
    )

    while (nextTime.forall(_ < bestTime)) {
      bestTime = nextTime.getOrElse(bestTime)

      val (delta0, delta1, time) = increments.map { case (si, ei) =>
        (si, ei, pathTime((frontHalf ++ Seq(add(p0, si), add(p1, ei)) ++ backHalf):_*))
      }.minBy(_._3)
      nextTime = Some(time)
      if (time < bestTime) {
        p0 = add(p0, delta0)
        p1 = add(p1, delta1)
      }
    }

    if (bestTime < inputTime) {
      frontHalf ++ Seq(p0, p1) ++ backHalf
    } else {
      soFar
    }
  }

  class PathIterator (from: (Int, Int), to: (Int, Int), length: Int) extends Iterator[Seq[(Int, Int)]] {
    private val deltaX = to._1 - from._1
    private val maxY = deltaX + to._2 -1
    private val midX = (to._1 + from._1) / 2
    var lastNextPt: Option[(Int, Int)] = Some(from)
    var miderator: Option[PathIterator] = advanceMiderator(false)
    var lastTime: Option[Double] = None

    private def advanceMiderator (mandatoryNextX: Boolean) = {
      if (length <= 2 || lastNextPt.isEmpty) None
      else {
        val (lx, ly) = lastNextPt.get
        if (ly < maxY && !mandatoryNextX) {
          lastNextPt = Some((lx, ly + 1))
        } else if (lx < midX) {
          lastNextPt = Some((lx + 1, from._2))
          lastTime = None
        } else lastNextPt = None

        lastNextPt.map { lnp =>
          new PathIterator(lnp, (to._1 - (lnp._1 - from._1), lnp._2), length - 2)
        }
      }
    }
    override def hasNext: Boolean = miderator.exists(_.hasNext) || lastNextPt.nonEmpty

    override def next(): Seq[(Int, Int)] = {
      val result = (from +: miderator.map(_.next()).getOrElse(Seq()) :+ to)
      val resultTime = pathTime(result:_*)
      if (lastTime.exists(_ < resultTime)) {
        miderator = advanceMiderator(true)
      } else {
        lastTime = Some(resultTime)
        if (2 == length) {
          lastNextPt = None
          miderator = None
        } else if (!miderator.exists(_.hasNext)) {
          miderator = advanceMiderator(false)
        }
      }
      result
    }
  }

  def jumpSearch (path: Seq[(Int, Int)], maxDistance: Int): Seq[(Int, Int)] = {
    val startTime = pathTime(path:_*)
    val half = path.length / 2
    val frontHalf = path.take(half)
    val backHalf = path.drop(half)
    val lastFront = frontHalf.last
    val firstBack = backHalf.head

    (1 to maxDistance).map { n =>
      ((0 to n).map(nn => (nn, n)) ++ (n-1 to 0 by -1).map(nn => (n, nn)))
        .map(delta => frontHalf ++ Seq(add(lastFront, delta), add(firstBack, (-delta._1, delta._2))) ++ backHalf)
        .map(newPath => (newPath, pathTime(newPath:_*)))
        .filter(_._2 < startTime)
        .reduceOption((a, b) => if (a._2 < b._2) a else b)
    }
      .find(_.isDefined)
      .flatten
      .map(_._1)
      .getOrElse(path)
  }

  def walkSearch (soFar: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    val inputTime = pathTime(soFar:_*)
    // Insert a pair of points, 1 up and in from the centers
    val halfSize = soFar.length / 2

    val frontHalf = soFar.take(halfSize)
    val front = frontHalf.dropRight(1)
    var p0 = frontHalf.last

    val backHalf = soFar.drop(halfSize)
    val back = backHalf.drop(1)
    var p1 = backHalf.head

    var bestTime = inputTime
    var nextTime: Option[Double] = None

    val increments = Seq(
      ((0, 0), (0, 0)),
      ((1, 0), (-1, 0)),
      ((1, 1), (-1, 1)),
      ((0, 1), (0, 1))
    )

    while (nextTime.forall(_ < bestTime)) {
      bestTime = nextTime.getOrElse(bestTime)

      val (delta0, delta1, time) = increments.map { case (si, ei) =>
        (si, ei, pathTime((front ++ Seq(add(p0, si), add(p1, ei)) ++ back):_*))
      }.minBy(_._3)
      nextTime = Some(time)
      p0 = add(p0, delta0)
      p1 = add(p1, delta1)
    }

    if (bestTime < inputTime) {
      front ++ Seq(p0, p1) ++ back
    } else {
      soFar
    }
  }

  def main (args: Array[String]): Unit = {
    jumpWalkSearch()
    walkSearch()
    iteratorSearch()
  }

  def jumpWalkSearch (): Unit = {
    var seq = Seq((0, 1), (10, 1))
    var bestTime = pathTime(seq:_*)
    var continue = true
    while (continue) {
      val nextSeq = walkSearch(jumpSearch(seq, 10))
      val newTime = pathTime(nextSeq:_*)
      if (newTime >= bestTime) {
        continue = false
      } else {
        seq = nextSeq
        bestTime = newTime
        println(s"${seq}: ${bestTime}")
      }
    }
  }

  def walkSearch (): Unit = {
    var lastSeq = Seq((0, 1), (10, 1))
    var lastTime = pathTime(lastSeq:_*)
    do {
      lastTime = pathTime(lastSeq:_*)
      println(s"Best so far: ${lastSeq}, at ${pathTime(lastSeq:_*)}")
      lastSeq = pathSearch(lastSeq)
    } while (pathTime(lastSeq:_*) < lastTime)

    // TODO: Alternate (search quarter) step and (scan by 1) step
  }

  def iteratorSearch () {
    var bestTime = Double.MaxValue
    val N = 10
    (2 to N by 2).foreach { n =>
      new PathIterator((0, 1), (N, 1), n).foreach { path =>
        val time = pathTime(path:_*)
        if (time < bestTime) {
          val deltas = (path.dropRight(1) zip path.drop(1)).map(p =>
            (p._2._1 - p._1._1, p._2._2 - p._1._2)
          )
          println(s"${path}: ${time}\tdeltas: ${deltas}")
          bestTime = time
        }
      }
    }
  }
}
