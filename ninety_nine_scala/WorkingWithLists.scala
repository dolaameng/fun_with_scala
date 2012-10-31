object WorkingWithLists {
	def last[T](xs : List[T]) : T = xs.last // xs.init
	def penultimate[T](xs: List[T]) : T = xs.init.last
	def nth[T](n: Int, xs: List[T]) : T = xs(n)
	def length[T](xs: List[T]) : Int = xs.length
	def reverse[T](xs: List[T]) : List[T] = xs.reverse
	def isPalindrome[T](xs: List[T]) : Boolean = 
		!(List.range(0, xs.length) 
			takeWhile (i => (i <= xs.length-1-i) && xs(i) == xs(xs.length-1-i))).isEmpty
	def flatten(nested: List[Any]) : List[Any] = nested flatMap {
		case ms: List[_] => flatten(ms) // type matching
		case e => List(e)
	}
	def compress[T](xs: List[T]) : List[T] = {
		xs.head :: (List.range(1, xs.length) flatMap 
			{i => if (xs(i) != xs(i-1)) Some(xs(i)) else None})
	}
	def compressFunctional[T](xs: List[T]) : List[T] = {
		xs.foldRight(List[T]()) { (x, accum) => 
			if (accum.isEmpty || x != accum.head) x::accum
			else accum
		}
	}
	def pack[T](xs: List[T]) : List[List[T]] = xs match {
		case Nil => List(List())
		case _ => {
			val (packed, next) = xs span (xs.head == _)
			if(next == Nil) List(packed) else packed :: pack(next)
		} 
	}
	def encode[T](xs: List[T]) : List[(Int, T)] = {
		if (xs == Nil) List()
		else {
			val (packed, next) = xs span (_ == xs.head)
			(packed.length, packed.head) :: encode(next)
		}
	}
	def encodeModified[T](xs: List[T]) : List[Any] = {
		if (xs == Nil) List[Any]()
		else {
			val (packed, next) = xs span (_ == xs.head)
			if (packed.length == 1) packed.head :: encodeModified(next)
			else (packed.length, packed.head) :: encodeModified(next)
		}
	}
	// entrance to tests
	def main(args: Array[String]) {
		val fibs = List(1, 1, 2, 3, 5, 8)
		val palindrome = List(1, 2, 3, 3, 1)
		// P01 Find the last element of a list
		assert (last(fibs) == 8)
		// P02 find the last but one element of a list
		assert (penultimate(fibs) == 5)
		// P03 find the Kth element of a list (offset = 0)
		assert (nth(2, fibs) == 2)
		// P04 find the number of elements of a list
		assert (length(fibs) == 6)
		// P05 reverse a list
		assert (reverse(fibs) == List(8, 5, 3, 2, 1, 1))
		// P06 find out whether a list is palindrome
		assert (isPalindrome(palindrome))
		assert (!isPalindrome(fibs))
		// P07 flatten a nested list structure
		assert (flatten(List(List(1, 1), 2, List(3, List(5, 8)))) 
					== fibs)
		// P08 eliminate consecutive duplicates of list elements
		assert (compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
			== List('a, 'b, 'c, 'a, 'd, 'e))
		assert (compressFunctional(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
			== List('a, 'b, 'c, 'a, 'd, 'e))
		// P09 pack consecutive duplicates of list elements into sublists
		assert (pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) 
			== List(List('a, 'a, 'a, 'a), 
					List('b), 
					List('c, 'c), 
					List('a, 'a), 
					List('d), 
					List('e, 'e, 'e, 'e)))
		// P10 run-length encoding of a list
		// run-length encoding data is compression method, 
		// where consecutive duplicates are encoded as tupels (N, E)
		assert (encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
			== List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
		// P11 modified run-length encoding
		// modify the solution to P10 in such a way that if an element
		// has no duplicates, it is simply copied into the result list
		assert (encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
			== List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))

		println("all tests passed...")
	}
}