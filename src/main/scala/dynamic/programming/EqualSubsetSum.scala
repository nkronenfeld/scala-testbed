package ndk
package dynamic.programming

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object EqualSubsetSum {
  def main (args: Array[String]): Unit = {
    test(divideSet, 1, "WARMUP", 1, 2, 3, 4)
    test(divideSet, 100, "brute-force", 1, 2, 3, 4)
    test(divideSet, 100, "brute-force", 1, 3, 4, 7)
    test(divideSet, 100, "brute-force", 2, 3, 4, 5)
    test(divideSetMemoized, 100, "memoized", 1, 2, 3, 4)
    test(divideSetMemoized, 100, "memoized", 1, 1, 3, 4, 7)
    test(divideSetMemoized, 100, "memoized", 2, 3, 4, 6)
    test(divideSetDP, 100, "dp", 1, 2, 3, 4)
    test(divideSetDP, 100, "dp", 1, 1, 3, 4, 7)
    test(divideSetDP, 100, "dp", 2, 3, 4, 6)

    val r = new Random(System.currentTimeMillis())
    (10 to 100 by 10).foreach { n =>
      val candidate = (1 to n).map(_ => r.nextInt(n))
      val bigList = (candidate.sum % 2) match {
        case 0 => candidate
        case 1 => (candidate.head + 1) +: candidate.tail
      }

      println(s"Trying ${n}-item list ${bigList}")
      println()

      if (n < 50) test(divideSet, 1, s"brute-force ${n}", bigList: _*)
      test(divideSetMemoized, 1, s"memoized ${n}", bigList: _*)
      test(divideSetDP, 1, s"arrayed ${n}", bigList: _*)
    }
  }

  def test (fcn: (Seq[Int]) => Option[(Seq[Int], Seq[Int])], iterations: Int, name: String, seq: Int*): Unit = {
    val (result, n0, n1, n2) = (1 to iterations).map { _ =>
      val startTime = System.nanoTime()
      val result = fcn(seq)
      val endTime = System.nanoTime()
      val elapsedTime = endTime - startTime
      (result, 1, elapsedTime, elapsedTime * elapsedTime)
    }.reduce((a, b) => (a._1, a._2 + b._2, a._3 + b._3, a._4 + b._4))

    val mean = n1.toDouble / n0.toDouble
    val sd = math.sqrt(n2.toDouble / n0.toDouble - mean * mean)
    println(s"Results for ${name}: ${result}")
    println(s"mean time: ${mean/1000000.0} ms")
    println(s"std. dev.: ${sd/1000000.0} ms")
    println()
  }

  def divideSet (set: Seq[Int]): Option[(Seq[Int], Seq[Int])] = {
    def rDiv (from: Seq[Int], set1: Seq[Int], set2: Seq[Int]): Option[(Seq[Int], Seq[Int])] = {
      if (from.isEmpty) {
        if (set1.sum == set2.sum) Some((set1, set2))
        else None
      } else {
        rDiv(from.tail, set1 :+ from.head, set2)
          .orElse(rDiv(from.tail, set1, set2 :+ from.head))
      }
    }
    rDiv(set, Seq(), Seq())
  }


  def divideSetMemoized (set: Seq[Int]): Option[(Seq[Int], Seq[Int])] = {
    // key is (index, difference)
    val seen = mutable.HashMap[(Int, Int), Option[(Seq[Int], Seq[Int])]]()

    def rDiv (from: Seq[Int], index: Int,
              left: Seq[Int], leftSum: Int,
              right: Seq[Int], rightSum: Int): Option[(Seq[Int], Seq[Int])] = {
      seen.getOrElseUpdate((index, rightSum - leftSum), {
        if (index >= from.length) {
          if (leftSum == rightSum) Some((left, right))
          else None
        } else {
          val next = from(index)

          (
            // left result
            if (leftSum + next < rightSum) {
              rDiv(from, index + 1, right, rightSum, left :+ next, leftSum + next)
            } else {
              rDiv(from, index + 1, left :+ next, leftSum + next, right, rightSum)
            }
          ).orElse(
            // right result
            if (rightSum + next < leftSum) {
              rDiv(from, index + 1, right :+ next, rightSum + next, left, leftSum)
            } else {
              rDiv(from, index + 1, left, leftSum, right :+ next, rightSum + next)
            }
          )
        }
      })
    }

    rDiv(set, 0, Seq(), 0, Seq(), 0)
  }

  def divideSetDP (set: Seq[Int]): Option[(Seq[Int], Seq[Int])] = {
    val sum = set.sum
    if (sum % 2 == 1) {
      None
    } else {
      val size = set.length
      val target = sum / 2
      val tests = (Array.fill(target + 1)(false) +: set.indices.map(_ => Array.fill(target + 1)(false))).toArray
      tests(0)(0) = true

      def fill (index: Int): Unit = {
        tests(index).indices.foreach { n =>
          tests(index + 1)(n) = tests(index)(n) || (n >= set(index) && tests(index)(n - set(index)))
        }
      }
      set.indices.foreach(fill)

      if (tests(size)(target)) {
        @tailrec
        def getLeftRight(index: Int, sum: Int, left: Seq[Int], right: Seq[Int]): (Seq[Int], Seq[Int]) = {
          if (index < 0) (left, right)
          else {
            if (tests(index + 1)(sum) == tests(index)(sum)) {
              getLeftRight(index - 1, sum, left, set(index) +: right)
            } else {
              getLeftRight(index - 1, sum - set(index), set(index) +: left, right)
            }
          }
        }

        Some(getLeftRight(size - 1, target, Seq(), Seq()))
      } else {
        None
      }
    }
  }
}
