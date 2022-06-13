package ndk



import org.scalatest.funspec.AnyFunSpec



class PrimesTest extends AnyFunSpec {
  describe("basic prime determination") {
    it("should be able to find all the primes < 30") {
      assert(Seq(2, 3, 5, 7, 11, 13, 17, 19, 23, 29) === Primes.primesTo(30))
    }
  }

  describe("new primes method") {
    it("should get the same results for primes to a number as the old method") {
      assert(OldPrimes.primesTo(1000) === Primes.primesTo(1000))
    }

    it("should get the same results for the nth prime as the old method") {
      Seq(12, 14, 20, 6, 7, 13, 23).foreach { n =>
        assert(OldPrimes.nthPrime(n) === Primes.nthPrime(n), s"Differing values for ${n}")
      }
    }

    it("should get the same primes with or without factor calculations") {
      Seq(12, 14, 20, 6, 7, 13, 23).foreach { n =>
        assert(Primes.nthPrime(n) === PrimesNoFactors.nthPrime(n), s"Differing values for ${n}")
      }
    }
    it("should be faster than the brute-force method") {
      val oldStartTime = System.nanoTime()
      OldPrimes.isPrime(1000000)
      val oldEndTime = System.nanoTime()
      println(s"Brute force time: ${(oldEndTime - oldStartTime) / 1000000.0}ms")

      val newStartTime = System.nanoTime()
      Primes.isPrime(1000000)
      val newEndTime = System.nanoTime()
      println(s"New time: ${(newEndTime - newStartTime) / 1000000.0}ms")

      assert((newEndTime - newStartTime) < (oldEndTime - oldStartTime))
    }
  }
  describe("problem 2") {
    it("our problem") {
      val target = 600851475143L
      val maxFactor = math.floor(math.sqrt(target)).toInt
      println(Primes.primesTo(maxFactor).filter(n => 0 === target % n))
    }
  }

  describe("problem 7") {
    it("should calculate the nth prime") {
      assert(2 === Primes.nthPrime(1))
      assert(3 === Primes.nthPrime(2))
      assert(29 === Primes.nthPrime(10))
    }
    ignore("should solve our problem") {
      println("The 10001th prime is " + Primes.nthPrime(10001))
    }
  }

  describe("factorization") {
    it("should get the factors of a prime") {
      assert(Seq(13) === Primes.factors(13))
      assert(Seq(17) === Primes.factors(17))
      assert(Seq(19) === Primes.factors(19))
    }

    it("should get the factors of a non-prime") {
      assert(Seq(2, 2, 5, 7) === Primes.factors(2 * 2 * 5 * 7).sorted)
      assert(Seq(3, 5, 5, 11) === Primes.factors(3 * 5 * 5 * 11).sorted)
      assert(Seq(2, 3, 5, 7, 11, 13, 17) === Primes.factors(2 * 3 * 5 * 7 * 11 * 13 * 17).sorted)
      assert(Seq(41, 43) === Primes.factors(41 * 43).sorted)
      assert(Seq(41, 41) === Primes.factors(41 * 41).sorted)
    }
  }
}
