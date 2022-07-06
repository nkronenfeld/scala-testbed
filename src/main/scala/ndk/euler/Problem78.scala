package ndk
package ndk.euler



import util.Timing
import util.PositiveIntegerIterator

import scala.collection.mutable



object Problem78 {
  private val maxCheck = 100000
  def main(args: Array[String]): Unit = Timing.time {
    val startTime = System.nanoTime()
    (1 to maxCheck).foreach { n =>
      val pn = partitionsByPentagonals(n)
      if (0 == (pn % 1000000)) {
        println(s"The number of partitions of ${n}, ${pn}, is divisible by 1000000")
      } else if (0 == n % 1000) {
        println(s"Done checking to ${n} in ${(System.nanoTime() - startTime) / 1000000.0}ms")
      }
    }
    println(s"Done checking to ${maxCheck}")
  }


  def pentagonalsUntilWithSign (n: Int): Seq[(Int, Int)] = {
    new PositiveIntegerIterator().flatMap { a =>
      val sign = if (0 == (a % 2)) -1 else 1
      Seq((a * (3 * a - 1) / 2, sign), (-a * (3 * -a - 1) / 2, sign))
    }.takeWhile(_._1 <= n).toSeq
  }
  val pentagonals = pentagonalsUntilWithSign(maxCheck)

  private val partitionsByPentsCache = {
    val foo = mutable.HashMap[Int, BigInt]()
    foo(0) = 1
    foo(1) = 1
    foo
  }

  def partitionsByPentagonals (n: Int): BigInt = partitionsByPentsCache.getOrElseUpdate(n, {
    val a = pentagonals.takeWhile(_._1 <= n)
    val b = a.map { case (nn, sign) => partitionsByPentagonals(n - nn) * sign }
    val c = b.sum
    c
  })


//  private val partitionCounts = mutable.HashMap[Int, BigInt]()
//  def partitionsByPiles(n: Int): BigInt = partitionCounts.getOrElseUpdate(n, {
//    val byNumPiles = (1 to n).map { p =>
//      val a = pilesFromN(n, p)
//      val b = pentagonalsUntilWithSign(n).map { case (p, sign) => sign * partitionsByPiles(n - p) }
//      val c = b.sum
//      a
//    }
//    byNumPiles.sum
//  })
//
//  private val pfn = {
//    val foo = (1 to maxCheck).map(n => Array.fill(n)(BigInt.int2bigInt(0))).toArray
//    foo(0)(0) = 1
//    foo
//  }
//
//  var maxN: Int = 0
//  var hits: Long = 0
//  var misses: Long = 0
//
//  // Number of ways N identical items can be broken up into P piles
//  def pilesFromN(n: Int, p: Int): BigInt = {
//    val attempt = pfn(n - 1)(p - 1)
//    if (attempt > 0) {
//      attempt
//    } else {
//      val r: BigInt = {
//        if (p > n) 0
//        else if (p == n) 1
//        else if (p == 1) 1
//        else {
//          // Put 1 in each pile
//          val left = n - p
//
//          val subPiles = (1 to math.min(left, p)).map(subPiles => pilesFromN(left, subPiles))
//          subPiles.sum
//        }
//      }
//      pfn(n-1)(p-1) = r
//      r
//    }
//  }
}
