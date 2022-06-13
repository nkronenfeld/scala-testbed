package ndk
package dynamic.programming

import scala.collection.mutable

// from https://www.educative.io/courses/grokking-dynamic-programming-patterns-for-coding-interviews/RM1BDv71V60
object BestBasket {
  val data = Seq(Item("Apple", 2, 4), Item("Orange", 3, 5), Item("Banana", 1, 3), Item("Melon", 4, 7))
  val bigData = Seq(
    Item("Granny Smith",  300, 12 * 1.3),
    Item("Red Delicious", 275, 11 * 1.2),
    Item("Cavendish",     125,  5 * 1.0),
    Item("Navel",         350, 14 * 1.4),
    Item("Mandarin",      150,  6 * 1.1),
    Item("Honeydew",     1500, 60 * 1.7),
    Item("Canteloupe",   1000, 40 * 1.6),
    Item("Santa Claus",   750, 30 * 1.5)
  )

  def fillWith (item: Item, knapsack: Seq[Item], maxWeight: Int): Seq[Item] = {
    if (item.weight + knapsack.map(_.weight).sum <= maxWeight) {
      fillWith(item, knapsack :+ item, maxWeight)
    } else {
      knapsack
    }
  }



  def main (args: Array[String]): Unit = {
    // val possibleItems = data.flatMap(item => fillWith(item, Seq(), knapsackSize))
    // val possibleItems = data
    val possibleItems = bigData


    // warm-up
    test(() => BruteForceFillAlgorithm, "WARM-UP", 2000, possibleItems, 1)

    test(() => BruteForceFillAlgorithm, "brute force", 2000, possibleItems, 100)
    test(() => BruteForceFillAlgorithm, "brute force", 2500, possibleItems, 100)
    test(() => BruteForceFillAlgorithm, "brute force", 3000, possibleItems, 100)
    test(() => RecursiveFillAlgorithm, "bare recursive", 2000, possibleItems, 100)
    test(() => RecursiveFillAlgorithm, "bare recursive", 2500, possibleItems, 100)
    test(() => RecursiveFillAlgorithm, "bare recursive", 3000, possibleItems, 100)
    test(() => new RecursiveFillAlgorithmWithMemoization, "memoized recursive", 2000, possibleItems, 100)
    test(() => new RecursiveFillAlgorithmWithMemoization, "memoized recursive", 2500, possibleItems, 100)
    test(() => new RecursiveFillAlgorithmWithMemoization, "memoized recursive", 3000, possibleItems, 100)
  }

  def test (getAlgorithm: () => FillAlgorithm, name: String, maxContents: Int, possibleItems: Seq[Item], iterations: Int): Unit = {
    val (result, n0, n1, n2) = (1 to iterations).map { _ =>
      val algorithm = getAlgorithm()
      val startTime = System.nanoTime()
      val results = algorithm.fill(Knapsack(maxContents, maxContents, Seq()), possibleItems)
      val endTime = System.nanoTime()
      val elapsedTime = endTime - startTime
      (results, 1, elapsedTime, elapsedTime * elapsedTime)
    }.reduce((a, b) => (a._1, a._2 + b._2, a._3 + b._3, a._4 + b._4))

    println(s"The result of algorithm ${name} is ${result} with profit ${result.profit} and weight ${result.weight}")
    println(s"Total time elapsed: ${n1/1000000.0}ms")
    val mean = n1.toDouble / n0.toDouble
    println(s"Mean time per run: ${mean/1000000.0}ms")
    println(s"Standard deviation: ${math.sqrt(n2.toDouble / n0.toDouble - mean * mean) / 1000000.0}ms")
    println
  }

}

case class Item (name: String, weight: Int, profit: Double)

case class Knapsack (totalCapacity: Int, spareCapacity: Int, contents: Seq[Item]) {
  def add(items: Seq[Item]): Knapsack =
    Knapsack(totalCapacity, spareCapacity - items.map(_.weight).sum, contents ++ items)

  def combinedWith (that: Knapsack): Knapsack = {
    Knapsack(this.totalCapacity + that.totalCapacity, this.spareCapacity + that.spareCapacity, this.contents ++ that.contents)
  }
  lazy val profit: Double = contents.map(_.profit).sum
  lazy val weight: Int = contents.map(_.weight).sum
}

trait FillAlgorithm {
  def fill (knapsack: Knapsack, from: Seq[Item]): Knapsack
}

object BruteForceFillAlgorithm extends FillAlgorithm {
  override def fill(knapsack: Knapsack, from: Seq[Item]): Knapsack = {
    val possibleCombos = (1 to knapsack.spareCapacity).flatMap(n => from.combinations(n))
      .filter(_.map(_.weight).sum <= knapsack.spareCapacity)

    knapsack.add(possibleCombos.maxBy(_.map(_.profit).sum))
  }
}

object RecursiveFillAlgorithm extends FillAlgorithm {
  private def innerRecurse (items: Seq[Item], index: Int, capacity: Int): (Seq[Item], Double) = {
    if (capacity <= 0 || index >= items.length) (Seq(), 0)
    else if (items(index).weight > capacity) innerRecurse(items, index + 1, capacity)
    else {
      val ourItem = items(index)
      val (otherItems, otherProfit) = innerRecurse(items, index + 1, capacity - ourItem.weight)
      Seq(
        (ourItem +: otherItems, ourItem.profit + otherProfit),
        innerRecurse(items, index + 1, capacity)
      ).maxBy(_._2)
    }
  }

  override def fill(knapsack: Knapsack, from: Seq[Item]): Knapsack = {
    val (newItems, _) = innerRecurse(from, 0, knapsack.spareCapacity)

    knapsack.add(newItems)
  }
}

class RecursiveFillAlgorithmWithMemoization extends FillAlgorithm {
  private val memoization = mutable.HashMap[(Int, Int), (Seq[Item], Double)]()
  private def innerRecurse (items: Seq[Item], index: Int, capacity: Int): (Seq[Item], Double) = {
    memoization.getOrElseUpdate((index, capacity),
      if (capacity <= 0 || index >= items.length) (Seq(), 0)
      else if (items(index).weight > capacity) innerRecurse(items, index + 1, capacity)
      else {
        val ourItem = items(index)
        val (otherItems, otherProfit) = innerRecurse(items, index + 1, capacity - ourItem.weight)
        Seq(
          (ourItem +: otherItems, ourItem.profit + otherProfit),
          innerRecurse(items, index + 1, capacity)
        ).maxBy(_._2)
      }
    )
  }

  override def fill(knapsack: Knapsack, from: Seq[Item]): Knapsack = {
    val (newItems, _) = innerRecurse(from, 0, knapsack.spareCapacity)

    knapsack.add(newItems)
  }
}