package ndk
package util

class PositiveIntegerIterator extends Iterator[Int] {
  var value = 1

  override def hasNext: Boolean = true

  override def next(): Int = {
    value += 1
    (value - 1)
  }
}
