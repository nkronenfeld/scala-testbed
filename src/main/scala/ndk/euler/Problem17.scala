package ndk
package ndk.euler

object Problem17 {
  def main (args: Array[String]): Unit = {
    println(s"1 to 5: ${(1 to 5).map(numberToString).map(countLetters).sum}")
    println(s"342 (${numberToString(342)}): ${countLetters(numberToString(342))}")
    println(s"115 (${numberToString(115)}): ${countLetters(numberToString(115))}")
    (1 to 1000).map(numberToString).foreach(println)
    println(
      (1 to 1000)
        .map(numberToString)
        .map(countLetters)
        .sum
    )
  }



  private val singleDigits = Array("", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  private val teens = Array("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
  private val tens = Array("", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")

  def countLetters (s: String): Int = s.count(_ != ' ')
  def numberToString (n: Int): String = {
    assert(n < 10000)

    val digits = n.toString.split("")

    val topDigits =
      digits.zipWithIndex.map { case (digit, index) => (digit.toInt, digits.length - index) }
        .flatMap { case (digit, place) =>
          place match {
            case 4 => // thousands
              if (digit > 0) {
                Some(singleDigits(digit) + " thousand")
              } else None
            case 3 => // hundreds
              if (digit > 0) {
                Some(singleDigits(digit) + " hundred")
              } else None
            case _ =>
              None
          }
        }.mkString(", ")

    val onesPlace = digits.last.toInt
    val tensPlace = if (digits.length > 1) digits.dropRight(1).last.toInt else 0

    val bottomDigits = {
      if (tensPlace == 0) {
        singleDigits(onesPlace)
      } else if (tensPlace == 1) {
        teens(onesPlace)
      } else if (onesPlace == 0) {
        tens(tensPlace)
      } else {
        tens(tensPlace) + " " + singleDigits(onesPlace)
      }
    }

    if (topDigits.nonEmpty && bottomDigits.nonEmpty) {
      topDigits + " and " + bottomDigits
    } else {
      topDigits + bottomDigits
    }
  }
}
