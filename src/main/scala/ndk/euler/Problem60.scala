package ndk
package ndk.euler

import scala.collection.mutable

object Problem60 {
  private val targetLength = 5
  private val pairs = mutable.HashMap[Int, mutable.ArrayBuffer[Int]]()


  def checkTarget (known: Int, others: Seq[Seq[Int]]): Option[Seq[Int]] = {
    others.find { quad =>
      quad.combinations(2).forall { pair => pairs(pair.head).contains(pair.last) }
    }.map(_ :+ known)
  }

  def add (a: Int, b: Int): Unit = {
    pairs.getOrElseUpdate(a, mutable.ArrayBuffer[Int]()).addOne(b)
    pairs.getOrElseUpdate(b, mutable.ArrayBuffer[Int]()).addOne(a)
  }

  def main (args: Array[String]): Unit = {
    val startTime = System.currentTimeMillis()
    var i = 1
    var found = false
    while (!found) {
      val bs = PrimesNoFactors.firstNPrimes(i)
      val a = bs.last
      print(s"Checking ${i}: ${a}: cp with [")
      bs.foreach(b =>
        if (PrimesNoFactors.isPrime(concat(a, b)) && PrimesNoFactors.isPrime(concat(b, a))) {
          print(s" ${b}")
          add(a, b)
        }
      )

      if (pairs.contains(a) && pairs(a).length >= targetLength) {
        checkTarget(a, pairs(a).filter(b => pairs(b).length >= targetLength).combinations(targetLength - 1).toSeq.map(_.toSeq)) match {
          case None =>
          case Some(result) =>
            found = true
            println(s"\n\nquint: ${result}")
        }
      } else {
        None
      }

      println("]")
      i += 1
    }
    val endTime = System.currentTimeMillis()
    println(s"Elapsed time: ${(endTime - startTime) / 1000.0}s")
  }

  private def concat (a: Int, b: Int): Int = {
    Array.fill(math.log10(b).floor.toInt + 1)(10).product * a + b
  }
}
