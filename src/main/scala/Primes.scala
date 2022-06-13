package ndk



import scala.collection.mutable

import scala.collection.mutable.ArrayBuffer



object OldPrimes {
  private val squaresSet = mutable.Set[Int]()
  private var maxSquareRoot = -1
  private var maxSquare = -1

  def isSquare (n: Int): Boolean = {
    while (maxSquare < n) {
      maxSquareRoot = maxSquareRoot + 1
      maxSquare = maxSquareRoot * maxSquareRoot
      squaresSet.addOne(maxSquare)
    }
    squaresSet.contains(n)
  }

  private val primeList = {
    val buffer = mutable.ArrayBuffer[Int]()
    buffer.addAll(Seq(2, 3))
    buffer
  }

  private def nextPrime: Unit =
    primeList.addOne(
      Range(primeList.last + 2, Integer.MAX_VALUE).find(n => !primeList.exists(p => n % p == 0)).get
    )

  def primesTo (n: Int): Seq[Int] = {
    while (primeList.last < n) nextPrime
    primeList.takeWhile(_ <= n).toSeq
  }

  def isPrime (n: Int): Boolean = {
    while (primeList.last < n) nextPrime
    primeList.contains(n)
  }

  def nthPrime (n: Int): Int = {
    while (primeList.size < n) nextPrime
    primeList(n - 1)
  }

  def factors (n: Int): Seq[Int] = {
    if (1 == n) Seq() else factorMap.remove(n) match {
      case Some(result) =>
        factorMap.put(n, result)
        result;
      case None =>
        val distinctFactors = primesTo(math.sqrt(n).floor.toInt).filter(p => 0 == n % p)
        val product = distinctFactors.product
        val result = if (1 == product) Seq(n) else distinctFactors ++ factors(n / product)
        factorMap.put(n, result)
        if (factorMap.size > maxSize) {
          factorMap.remove(factorMap.keysIterator.next())
        }
        result
    }
  }

  private val maxSize = 100000
  private val factorMap = mutable.LinkedHashMap[Int, Seq[Int]]()
}

object PrimesNoFactors {
  // indices here are offset by 2 - so element 0 represents the factors of 2
  private val isPrimeLists = new ArrayBuffer[Boolean]()
  private var primesFound: Int = 0

  // Initialize with our first 2 primes
  isPrimeLists.append(true)
  isPrimeLists.append(true)
  primesFound = 2

  private def extendTo (n: Int): Unit = {
    (isPrimeLists.length + 2 to n).foreach { nn =>
      val isPrime = !(2 to math.sqrt(nn).floor.toInt)
        .exists(nnn => isPrimeLists.apply(nnn - 2) && nn % nnn == 0)
      isPrimeLists.append(isPrime)
      if (isPrime) primesFound += 1
    }
  }

  def isPrime (n: Int): Boolean = {
    extendTo(n)
    isPrimeLists(n - 2)
  }

  def primesTo (n: Int): Seq[Int] = {
    extendTo(n)
    (2 to n).filter(n => isPrimeLists(n - 2))
  }

  def firstNPrimes (n: Int): Seq[Int] = {
    if (n > primesFound) {
      while (primesFound < n) {
        isPrime(isPrimeLists.length + 2)
      }
    }

    val allIndices: LazyList[Int] = {
      def loop(n: Int): LazyList[Int] = n #:: loop(n + 1)
      loop(0)
    }

    allIndices.filter(isPrimeLists.apply).take(n).map(_ + 2)
  }

  def nthPrime (n: Int):  Int = {
    if (n <= primesFound) {
      val allIndices: LazyList[Int] = {
        def loop(n: Int): LazyList[Int] = n #:: loop(n + 1)
        loop(0)
      }

      allIndices.filter(isPrimeLists.apply).take(n).last + 2
    } else {
      while (primesFound < n) {
        isPrime(isPrimeLists.length + 2)
      }
      isPrimeLists.length + 1
    }
  }
}


object Primes {
  // indices here are offset by 2 - so element 0 represents the factors of 2
  private val factorLists = new mutable.ArrayBuffer[Seq[Int]]()
  private val isPrimeLists = new ArrayBuffer[Boolean]()
  private var primesFound: Int = 0

  // Initialize with our first 2 primes
  factorLists.append(Seq(2))
  isPrimeLists.append(true)
  factorLists.append(Seq(3))
  isPrimeLists.append(true)
  primesFound = 2

  private def extendTo (n: Int): Unit = {
    while (factorLists.length < n - 1) {
      val nn = factorLists.length
      val factorList = factorize(nn + 2)
      factorLists.append(factorList)
      isPrimeLists.append(factorList.length == 1)
      if (factorList.length == 1) primesFound += 1
    }
  }

  def factors (n: Int): Seq[Int] = {
    extendTo(n)
    factorLists(n - 2)
  }

  def isPrime (n: Int): Boolean = {
    extendTo(n)
    isPrimeLists(n - 2)
  }

  def primesTo (n: Int): Seq[Int] = {
    extendTo(n)
    (2 to n).filter(n => isPrimeLists(n - 2))
  }

  def firstNPrimes (n: Int): Seq[Int] = {
    if (n > primesFound) {
      while (primesFound < n) {
        isPrime(factorLists.length + 2)
      }
    }

    val allIndices: LazyList[Int] = {
      def loop(n: Int): LazyList[Int] = n #:: loop(n + 1)
      loop(0)
    }

    allIndices.filter(isPrimeLists.apply).take(n).map(_ + 2)
  }

  def nthPrime (n: Int):  Int = {
    if (n <= primesFound) {
      val allIndices: LazyList[Int] = {
        def loop(n: Int): LazyList[Int] = n #:: loop(n + 1)
        loop(0)
      }

      allIndices.filter(isPrimeLists.apply).take(n).last + 2
    } else {
      while (primesFound < n) {
        isPrime(factorLists.length + 2)
      }
      factorLists.length + 1
    }
  }

  private def factorize(n: Int): Seq[Int] = {
    var i = 2
    var primeFactor = Option.empty[Int]
    while (i * i <= n && primeFactor.isEmpty) {
      if (isPrimeLists(i - 2)) {
        if (0 == n % i) {
          primeFactor = Some(i)
        }
      }
      i += 1
    }

    primeFactor match {
      case Some(p) =>
        factorLists(n/p - 2) :+ p
      case None => Seq(n)
    }
  }
}