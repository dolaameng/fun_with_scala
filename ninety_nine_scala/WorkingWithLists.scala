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
	def decode[T](encoded: List[(Int, T)]) : List[T] = {
	    encoded flatMap {t => List.fill(t._1)(t._2)}
	}
	def duplicate[T](xs: List[T]) : List[T] = {
	    xs flatMap {x => List.fill(2)(x)}
	}
	def duplicateN[T](n: Int)(xs: List[T]) : List[T] = {
	    xs flatMap {x => List.fill(n)(x)}
	}
	def drop[T](n: Int)(xs: List[T]) : List[T] = {
	    /*
	    for {
	        i <- List.range(0, xs.length)
	        if i % n != n - 1
	    } yield xs(i)
	    */
	    xs.zipWithIndex filter (_._2 % n != n-1) map (_._1)
	}
	def dropRecursive[T](n: Int)(xs: List[T]) : List[T] = {
	    // def another inner function to create clousre to keep n
	    def dropR[T](c: Int, ys: List[T]) : List[T] = (c, ys) match {
	        case (_, Nil) => Nil
	        case (1, _ :: tail) => dropR(n, tail)
	        case (_, head :: tail) => head :: dropR(c - 1, tail)
	    }
	    dropR(n, xs)
	}
	def dropRecursiveTail[T](n: Int)(xs: List[T]) : List[T] = {
	    def dropR[T](partial: List[T], c: Int, ys: List[T]) : List[T] = 
	        (c, ys) match {
	        case (_, Nil) => partial
	        case (1, _ :: tail) => dropR(partial, n, tail)
	        case (_, head :: tail) => dropR(partial++List(head), c-1, tail)
	    }
	    dropR(List(), n, xs)
	}
	def split[T](n: Int)(xs: List[T]) : (List[T], List[T]) = {
	    //(xs.slice(0, n), xs.slice(n, xs.length))
	    (xs.take(n), xs.drop(n))
	}
	def splitRecursive[T](n: Int)(xs: List[T]) : (List[T], List[T]) = (n, xs) match {
	    case (_, Nil) => (Nil, Nil)
	    case (0, _) => (Nil, xs)
	    case (_, head :: tail) => {
	        val (lhs, rhs) = splitRecursive(n-1)(tail)
	        (head::lhs, rhs)
	    }
	}
	def splitRecursiveTail[T](n: Int)(xs: List[T]) : (List[T], List[T]) = {
	    def lhsR[T](lhs: List[T], c: Int, ys: List[T]) : (List[T], List[T]) = (c, ys) match {
	        case (_, Nil) => (lhs, Nil)
	        case (0, _) => (lhs, ys)
	        case (_, head :: tail) => lhsR(lhs++List(head), c-1, tail)
	    }
	    lhsR(List(), n, xs)
	}
	def slice[T](start: Int, end: Int)(xs: List[T]) : List[T] = {
	    def sliceR(partial: List[T], i: Int, sublist: List[T]) : List[T] = (i, sublist) match {
	        case (_, Nil) => partial
	        case (_, _ :: tail) if (i < start) => sliceR(partial, i+1, tail)
	        case (_, h :: tail) if (i >= start && i < end) => sliceR(partial++List(h), i+1, tail)
	        case (_, _) if (i >= end) => partial
	        case (_, _) => throw new Exception("Impossible case")
	    }
	    sliceR(List(), 0, xs)
	}
	// rotate to the right (positive offset) or to the left (negative offset)
	def rotate[T](offset: Int)(xs: List[T]) : List[T] = {
	    val splitPoint = (if (offset < 0) -offset else xs.length - offset) % xs.length
	    xs.drop(splitPoint) ++ xs.take(splitPoint)
	}
	def removeAt[T](at : Int, xs: List[T]) : (List[T], T) = 
	    (xs.take(at) ++ xs.drop(at+1), xs(at))
	/*
	def insertAt[T](xx : T, at : Int, xs: List[T]) : List[T] = {
	    xs.slice(0, at) ++ (xx :: xs.slice(at, xs.length))
	}
	*/
	def insertAt[T](xx : T, at : Int, xs : List[T]) : List[T] = xs.splitAt(at) match {
	    case (left, right) => left ++ (xx :: right)
	}
	def sample[T](n : Int)(xs: List[T]) : List[T] = {
	    val randomIndices = (new util.Random).shuffle(List.range(0, xs.length))
	    randomIndices.take(n) map (xs(_))
	}
	//randomly generate n numbers from range 1..m
	def lotto(n: Int, m: Int) = {
	    (new util.Random).shuffle(List.range(1, m+1)).take(n) 
	}
	def combinations[T](xs : List[T])(n : Int) : List[List[T]] = (n, xs) match {
	    case (0, _) => List(List())
	    case (_, _) if n >= xs.length => List(xs)
	    case (_, head :: tail) => {
	        val withHead = combinations(tail)(n-1)
	        val withoutHead = combinations(tail)(n)
	        (withHead map (head::_)) ++ withoutHead
	    }
	}
	// divide 9 ppl into subgroups of size 2, 3 and 4
	// generate all possibilities
	def group3[T](xs: List[T]) : Iterator[List[List[T]]] = {
	    for {
	        a <- xs.combinations(2)
	        noA = xs filter (!a.contains(_))
	        b <- noA.combinations(3)
	        c = noA filter (!b.contains(_))
	    } yield List(a, b ,c)
	}
	def group[T](xs: List[T])(splits: List[Int]) : Iterator[List[List[T]]] = splits match {
	    case Nil => Iterator()
	    case _ :: Nil => Iterator(List(xs))
	    case n :: restSplits => {
	        for {
	            c <- xs.combinations(n)
	            rest = xs filter (!c.contains(_))
	            restC <- group(rest)(restSplits)
	        } yield c :: restC
	    }
	}
	def lsort[T](xs: List[List[T]]) : List[List[T]] =
	  xs sortBy (_.length)
	  
	def lsortFreq[T](xs: List[List[T]]) : List[List[T]] = 
	  (xs groupBy (_.length)).toList sortBy (_._2.length) flatMap (_._2)
	
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
		// P12 decode a run-length encoded list
		// uncompress the encoded list in P10 into a list
		assert (decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) 
		    == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
		// P13 run-length encoding of a list (direct solution)
		// it is already a "direct" implementation in encode
		// P14 Duplicate the elements of a list
		assert (duplicate(List('a, 'b, 'c, 'c, 'd)) 
		    == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
		// P15 duplicate the elements of a list a given number of times
		assert (duplicateN(3)(List('a, 'b, 'c, 'c, 'd)) 
		    == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
		// P16 Drop every Nth element from a list
		assert (drop(3)(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) 
		    == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
		assert (dropRecursive(3)(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) 
		    == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
		assert (dropRecursiveTail(3)(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) 
    		== List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
    	// P17 split a list into two parts
    	// the length of the first part is given, the result is a tuple
    	assert (split(3)(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    	    == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    	assert (splitRecursive(3)(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
        	== (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
        assert (splitRecursiveTail(3)(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
            == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
        // P18 extract a slice of a list
        // get the slice of list - elements bounded by index range [start, end)
        // index offset is 0
        assert (slice(3, 7)(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
            == List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k).slice(3, 7))
        // P19 rotate a list N places to the right
        assert (rotate[Symbol](-3)(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) 
            == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
        assert (rotate[Symbol](2)(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
            == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
        // P20 remove the kth element from a list
        // return the list and the removed element in a tuple
        assert (removeAt(1, List('a, 'b, 'c, 'd)) 
            == (List('a, 'c, 'd), 'b))
        // P21 insert an element at a given position into a list
        assert (insertAt('new, 1, List('a, 'b, 'c, 'd)) 
            == List('a, 'new, 'b, 'c, 'd))
        // P22 create a list containing all integers within a given range
        assert (List.range(2, 7) == List(2, 3, 4, 5, 6))
        // P23 extract a given number of randomly selected elements from a list
        val randomSample = sample(3)(List('a, 'b, 'c, 'd, 'f, 'g, 'h))
        assert (randomSample.length == 3)
        assert (randomSample.toSet.size == 3)
        // P24 Lotto: Draw N different random numbers from the set 1 .. M
        val winners = lotto(6, 49)
        assert (winners.length == 6)
        assert (winners.toSet.size == 6)
        assert ( (winners filter {w => (w <= 49 && w >= 1)}).length == 6)
        // P25 generate a random permutation of the elements of a list
        // use the shuffle of util.Random
        // P26 generate the combinations of K distinct objects chosen from 
        // the N elements of a list 
        val comb3 = combinations(List('a, 'b, 'c, 'd, 'e, 'f))(3)
        assert (comb3.toSet.size == 20)
        // P27 group the elements of a set into disjoint subsets - grouping
        // e.g., in how many ways can a group of 9 ppl work in 3 disjoint 
        // subgroups of 2, 3 and 4 persons, the function generate all 
        // possibilities
        val allGroups3 = group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")).toList
        for (groups <- allGroups3) groups match {
            case List(a, b, c) => {
                assert (a.length + b.length + c.length == 9)
                assert ((a++b++c).toSet == List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida").toSet)
            }
            case _ => throw new Exception("Impossible reach")
        }
        // generalize the above predicate in a way that we can specify a list 
        // of group sizes and the predicate will return a list of group
        val allGroups = group(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))(List(2, 3, 4)).toList
        assert (allGroups3.toSet == allGroups.toSet)
        // P28 sorting a list of lists according to length of sublists
        // Sort the sublists of lists according to their lengths. shorter -> longer
        // the orders should be kept for sublists of the same length
        assert (lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), 
                          List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
                == List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), 
                  List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l)))
        // sort the sublists according to their length frequency
        // Note that in the following example, 
        // the first two lists in the result have length 4 and 1 and both lengths appear just once. 
        // The third and fourth lists have length 3 and there are two list of this length. 
        // Finally, the last three lists have length 2. 
        // The order should be preserved at the same length frequencies
        assert (lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), 
                              List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), 
                              List('o))) 
          == List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), 
                  List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n)))
        

		println("all tests passed...")
	}
}