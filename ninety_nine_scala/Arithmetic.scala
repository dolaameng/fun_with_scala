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
    (n > 1) && (primes takeWhile (_ < Math.sqrt(n)) forall (n % _ != 0)) 
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
  
  //main test entry
  def main(args: Array[String]) {
    // P31 determine whether a given integer is prime
    assert (!isPrime(111))
    assert (!isPrime(16))
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
    
    println("all tests passed ...")
  }
}