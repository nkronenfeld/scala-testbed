package ndk
package util

object Timing {
  def time[T] (fcn: => T): T = {
    val startTime = System.nanoTime()
    val result: T = fcn
    val endTime = System.nanoTime()
    println(s"Elapsed time: ${(endTime - startTime)/1000000.0}ms")
    result
  }

  def returnTimeMs[T] (fcn: => T): (T, Double) = {
    val startTime = System.nanoTime()
    val result: T = fcn
    val endTime = System.nanoTime()
    (result, (endTime - startTime) / 1000000.0)
  }
}
