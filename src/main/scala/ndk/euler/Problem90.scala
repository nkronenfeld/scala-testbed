package ndk
package ndk.euler

object Problem90 {
  // 01 04 09 16 25 36 49 64 81
  // 0 1 2 3 4 5 6 8 9
  //
  // 0 vs 1 4 9
  // 1 vs 0 6 8
  // 2 vs 5
  // 3 vs 6
  // 4 vs 0 6 9
  // 5 vs 2
  // 6 vs 1 3 4
  // 8 vs 1
  // 9 vs 0 4
  //
  //
  // die 1          die 2
  // 2              5
  // others mutable

  val targets = Set("01", "04", "09", "16", "25", "36", "49", "64", "81")
  val inputs = Seq(0, 1, 2, 3, 4, 5, 6, 8, 9)

  def possibleDice (): Seq[(Seq[Int], Seq[Int])] = {
    (0 to 8).flatMap { a =>
      (a to 8).flatMap { b =>
        (b to 8).flatMap { c =>
          (c to 8).flatMap { d =>
            (d to 8).flatMap { e =>
              val d1 = Seq(2, inputs(a), inputs(b), inputs(c), inputs(d), inputs(e)).sorted
              (0 to 8).flatMap { a2 =>
                (a2 to 8).flatMap { b2 =>
                  (b2 to 8).flatMap { c2 =>
                    (c2 to 8).flatMap { d2 =>
                      (d2 to 8).map { e2 =>
                        (d1, Seq(5, inputs(a2), inputs(b2), inputs(c2), inputs(d2), inputs(e2)).sorted)
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }.distinct
  }

  def diceMatch (d1: Seq[Int], d2: Seq[Int]): Boolean = {
    (
      targets --
        d1.flatMap { v1 =>
          val v1s = if (v1 == 6 || v1 == 9) Seq(6, 9) else Seq(v1)
          d2.flatMap { v2 =>
            val v2s = if (v2 == 6 || v2 == 9) Seq(6, 9) else Seq(v2)
            v1s.flatMap(v1a => v2s.flatMap(v2a => Seq(v1a.toString + v2a.toString, v2a.toString + v1a.toString)))
          }
        }
      ).isEmpty
  }
  def main (args: Array[String]): Unit = {
    val dice = possibleDice().filter { case (d1, d2) => diceMatch(d1, d2) }
    dice.foreach(println)
    println(dice.length)
  }
}
