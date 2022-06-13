package ndk

object PalindromeNumbers {
  def isPalindrome (n: Int): Boolean = reverse(n) == n

  def digits (n: Int): Seq[Int] =
    if (n < 10) Seq(n)
    else PalindromeNumbers.digits(n/10) :+ (n % 10)

  def reverse (n: Int): Int =
    digits(n).reverse.foldLeft(0)((accum, next) => accum * 10 + next)
}
