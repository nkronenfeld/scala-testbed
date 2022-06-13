package ndk
package ndk.euler

object Problem9 {
  def main (args: Array[String]): Unit = {
    println(
      (1 to 1000).flatMap { aa =>
        (1 to 1000).map { b => (aa, b, 1000 - aa - b) }
          .filter { case (a, b, c) => a * a + b * b == c * c }
      }.map { case (a, b, c) => (a, b, c, a * b * c) }
    )
  }
}
