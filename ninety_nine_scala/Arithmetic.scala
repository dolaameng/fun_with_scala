object Arithmetic {
  // a fairly naive implemention for primality testing is simply
  // testing if it is divisable by any prime number less than or 
  // equal to its square root
  // Here the impl uses a Stream to create a lazy infinite list of 
  // prime numbers. The mutual recursion between primes and isPrime
  // because of the limit on isPrime to the sqrt of n beting tested
  // Ref: http://primes.utm.edu/prove/index.html
  // Ref: article.gmane.org/gmane.comp.lang.haskell.cafe/19470
  def isPrime(n: Int): Boolean = {
    val primes = Stream.cons(2, Stream.from(3, 2) filter (isPrime(_)))
    (n > 1) && (primes takeWhile (_ <= Math.sqrt(n)) forall (n % _ != 0)) 
  }
  def gcd(a: Int, b: Int) : Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }
  def isCoprime(a: Int, b: Int) = gcd(a, b) == 1
  def totient(m: Int): Int = {
    (List.range(1, m+1) filter (isCoprime(m, _))).length
  }
  // Recursion is more nature than while sometime
  def primeFactors(n: Int) : List[Int] = {
    val primes = Stream.cons(2, Stream.from(3, 2) filter (isPrime(_)))
    def primeFactorsR(nn: Int, ps: Stream[Int]) : List[Int] = {
      if (isPrime(nn)) List(nn)
      else if (nn % ps.head == 0) ps.head :: primeFactorsR(nn / ps.head, ps)
      else primeFactorsR(nn, ps.tail)
    }
    primeFactorsR(n, primes)
  }
  def primeFactorMultiplicity(n: Int) : List[(Int, Int)] = {
      //println((primeFactors(n) groupBy (p => p)).toList)
      ((primeFactors(n) groupBy (p => p)).values.toList 
        map {factors => (factors.head, factors.length)}
        sortBy (_._1))
  }
  def totientImproved(m: Int) : Int = {
      val primesMultiplicities = primeFactorMultiplicity(m)
      //println(m, primesMultiplicities)
      (primesMultiplicities map {
          case (p:Int, m:Int) => (p-1) * scala.math.pow(p, m-1).toInt
      }).product
  }
  def timeIt[T](label: String)(block: => T) : T = {
      val now = System.currentTimeMillis()
      val ret = block
      println(label + ": " + (System.currentTimeMillis() - now) + " ms.")
      ret
  } 
  def listPrimesInRange(range: Range) : List[Int] = {
      def isPrime(n: Int): Boolean = {
          val primes = Stream.cons(2, Stream.from(3, 2) filter (isPrime(_)))
          (n > 1) && (primes takeWhile (_ <= Math.sqrt(n)) forall (n % _ != 0))
      }
      (range filter (isPrime(_))).toList
      //(primes dropWhile (_ < range.head) takeWhile (_ <= range.last) ).toList
  }
  def goldbach(n: Int): (Int, Int) = {
      object Prime {
          val primes = Stream.cons(2, Stream.from(3, 2) filter (isPrime(_)))
          def isPrime(n: Int): Boolean = {
              (n > 1) && (primes takeWhile (_ <= Math.sqrt(n)) forall (n % _ != 0))
          }
      }
      // alter1
      //val p = (Prime.primes filter (p => isPrime(n - p))).toIterator.next
      //(p, n - p)
      // alter2
      (Prime.primes find (p => isPrime(n - p))) match {
          case None =>  throw new NoSuchElementException
          case Some(p) => (p, n-p)
      }
  }
  def goldbachList(range : Range) : List[(Int, (Int, Int))] = {
      range.toList filter (_ % 2 == 0) map (n => (n, goldbach(n)))
  }
  
  //main test entry
  def main(args: Array[String]) {
    // P31 determine whether a given integer is prime
    assert (!isPrime(111))
    assert (!isPrime(16))
    assert (!isPrime(4))
    assert (isPrime(1046527)) //Carol Prime
    // P32 determine the greatest common divisor of two positive integer numbers
    assert (gcd(36, 63) == 9)
    // P33 determine if two positive integers are coprime
    // two numbers are coprime is their gcd is 1
    assert (isCoprime(35, 64))
    // P34 calculate Euler totient function phi(m)
    // toient function phi(m) is defined as the number of postive ints r 
    // where 1 <= r <= m that are coprime to m
    assert (totient(10) == 4)
    // P35 determine the prime factors of a given positive integer
    assert (primeFactors(315) == List(3, 3, 5, 7))
    // P36 determine the prime factors of a given postive integer
    // Construct a list containing the prime factors and their multiplicity
    assert (primeFactorMultiplicity(315) 
        == List((3, 2), (5, 1), (7, 1)))
    // P37 calcualte Euler's totient function phi(m) IMPROVED than P34
    // Given the list of prime factors of a number m, the phi(m>) can be
    // efficiently calcualted as follows:
    // Let [(p1, m1), (p2, m2), ...] be the list of prime factors and their
    // multiplicities of a given number m. Then phi(m) can be calucated as
    // phi(m) = (p1-1) * p1 ^ (m1-1) * (p2-1) * p2 ^ (m2-1) * ...
    assert (totientImproved(10) == 4)
    for (i <- 10 to 20) {
        //println(i, totientImproved(i), totient(i))
        assert (totient(i) == totientImproved(i))
    }
    // P38 compare the performances of two version of totient
    assert(timeIt("totient(10090)")(totient(10090)) 
        == timeIt("totientImproved(10090)")(totientImproved(10090)))
    // P39 a list of prime numbers
    // given a range of integers by its lower and upper limit, construct
    // a list of all prime numbers in that range. 
    assert (listPrimesInRange(7 to 31)
        == List(7, 11, 13, 17, 19, 23, 29, 31))
    // P40 Goldbach's conjecture
    // Conjecture: Every positive even number greater than 2 is the sum of
    // of two prime numbers. e.g. 28 = 5 + 23
    // write a function to find the two prime numbers that sum up to a given int
    assert (List((5, 23)) contains goldbach(28))
    assert(List((3, 1000117)) contains goldbach(1000120))
    // P41 a list of Goldbach compositions
    // Given a range of integers by its lower and upper bounds
    // print a list of all even numbers and their Goldbach composition
    assert (goldbachList(9 to 20) ==
        List(
            (10, (3, 7)),
            (12, (5, 7)),
            (14, (3, 11)),
            (16, (3, 13)),
            (18, (5, 13)),
            (20, (3, 17))))
    
    println("all tests passed ...")
  }
}