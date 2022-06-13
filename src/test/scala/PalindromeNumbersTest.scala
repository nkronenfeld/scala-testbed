package ndk

import org.scalatest.funspec.AnyFunSpec

class PalindromeNumbersTest extends AnyFunSpec {
  describe("reverse") {
    it("should get the digits of a number") {
      assert(Seq(1, 2, 3, 4, 5) === PalindromeNumbers.digits(12345))
    }
    it("should reverse a number") {
      assert(12345 === PalindromeNumbers.reverse(54321))
    }
  }
  it("our problem") {
    println(
      Range(999, 901, -1).flatMap { a =>
        Range(a - 1, 900, -1).find { b =>
          PalindromeNumbers.isPalindrome(a * b)
        }.map(b => (a, b, a * b))
      }.maxBy(_._3)
    )
  }
}
