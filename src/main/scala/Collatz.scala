package ndk

import scala.annotation.tailrec
import scala.collection.mutable

object Collatz {
  private val knownSequences = mutable.HashMap[Long, Seq[Long]]()
  knownSequences(1) = Seq(1)

  def getSequence (n: Long): Seq[Long] =
    addResults(getSequence(n, Seq()))

  private def addResults (seq: Seq[Long]): Seq[Long] = {
    var seqLeft = seq
    while (!knownSequences.contains(seqLeft.head)) {
      knownSequences(seqLeft.head) = seqLeft
      seqLeft = seqLeft.tail
    }
    seq
  }

  @tailrec
  private def getSequence (n: Long, soFar: Seq[Long]): Seq[Long] = {
    if (knownSequences.contains(n)) {
      soFar ++ knownSequences(n)
    } else {
      val nn = (n % 2) match {
        case 0 => n / 2
        case 1 => 3 * n + 1
        case -1 =>
          throw new Exception("How did we get -1?")
      }
      getSequence(nn, soFar :+ n)
    }
  }

  def main (args: Array[String]): Unit = {
    println((1 to 1000000).maxBy(n => getSequence(n).length))
  }
}
