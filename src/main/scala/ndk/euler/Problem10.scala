package ndk
package ndk.euler

object Problem10 {
  def main (args: Array[String]): Unit = {
    println("Target: 1179908154")
    (1000 to 2000000 by 1000).foreach(n =>
      println(s"The sum of the primes up to ${n} is ${Primes.primesTo(n).map(_ + 0L).sum}")
    )
  }
}
