package ndk
package ndk.euler



class P15Graph (width: Int, height: Int) {
  private val nodes = Array.fill(width, height)(Option.empty[Long])
  nodes(width - 1)(height - 1) = Some(1)

  def entry (a: Int, b: Int): Long =
    nodes(a)(b) match {
      case Some(n) => n
      case None =>
        val result =
          (if (a < width - 1) entry(a + 1, b) else 0L) +
            (if (b < height - 1) entry(a, b + 1) else 0L)
        nodes(a)(b) = Some(result)
        result
    }
}

object Problem15 {
  def main (args: Array[String]): Unit = {
    (2 to 21).foreach { n =>
      val graph = new P15Graph(n + 1, n + 1)
      val result = graph.entry(0, 0)
      println(s"The number of paths in a ${n}x${n}-cell graph is ${result}")
    }
  }
}
