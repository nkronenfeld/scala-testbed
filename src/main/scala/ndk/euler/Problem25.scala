package ndk
package ndk.euler

import util.Timing


object Problem25 {

  def main (args: Array[String]): Unit = {
    Timing.time {
      var index = 3
      var last2 = (BigInt.int2bigInt(1), BigInt.int2bigInt(1))
      var curF = last2._1 + last2._2
      while (curF.toString().length < 1000) {
        index += 1
        last2 = (last2._2, curF)
        curF = last2._1 + last2._2
        println(curF.toString.length + ":  " + curF)
      }
      println(index)
    }
  }
}
