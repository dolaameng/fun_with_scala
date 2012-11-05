object Logic {
    def not(a: Boolean) : Boolean = a match {
        case true => false
        case false => true
    }
    def and(a: Boolean, b: Boolean) = (a, b) match {
        case (true, true) => true
        case (_, _) => false
    }
    def or(a: Boolean, b: Boolean) = (a, b) match {
        case (true, _) => true
        case (_, true) => true
        case (_, _) => false
    }
    def equ(a: Boolean, b: Boolean) = or(and(a, b), and(not(a), not(b)))
    def xor(a: Boolean, b: Boolean) = not(equ(a, b))
    def table2(predicate: (Boolean, Boolean) => Boolean): Unit = {
        val delimiter = "\t\t"
        println("a"+delimiter+"b"+delimiter+"result")
        for (a <- List(true, false); b <- List(true, false)) {
            println(a + delimiter + b + delimiter + predicate(a, b))
        }
    }
    def gray(n: Int): List[String] = n match {
        case 0 => List[String]("")
        case _ => {
            val gray_n_1 = gray(n-1)
            (gray_n_1 map ('0' + _)) ++ (gray_n_1.reverse map ('1' + _))
        }
    }
    
    // main test entry
    def main(args: Array[String]) : Unit = {
        // P46 truth tables for logical expressions
        // define functions and, or, nand, nor, xor impl and equ (for logical equivalence)
        // which return true or false according to the result their respecctive 
        // operations - implement them without using any built-in logic function
        assert (and(true, true))
        assert (xor(true, true) == false)
        
        // P47 truth tables for logic expressions (2)
        println("print-out for P47: ")
        table2( (a: Boolean, b: Boolean) => and(a, or(a, not(b))) )
        // print out:
        // A    B       result
        // true true    true
        // true false   true
        // false true   false
        // false false  false
        
        // P48 Truth tables for expressions(3)
        // Ommitted
        
        // P49 Gray Code
        // An n-bit gray code is a sequence of n-bit strings constructed according to
        // certain rules. Find out the rules based on the test examples
        // and write code to implement them
        // See if you can use memoziation to make the function more efficient
        assert (gray(1) == List("0", "1"))
        assert (gray(2) == List("00", "01", "11", "10"))
        assert (gray(3) == List("000", "001", "011", "010", "110", "111", "101", "100"))
        
        // P50 Huffman code
        // We suppose a list of symbols with their frequences, e.g.
        // List( (:a, 45), (:b, 13), (:c, 12), (:d, 16), (:e, 9), (:f, 5))
        // our objective is to construct a list of (S, C) tuples, where C 
        // is the Huffman code word for the symbol S
        //TODO: assert (huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
        //    == List(("a",0), ("b",101), ("c",100), ("d",111), ("e",1101), ("f",1100)))
        // homework of coursera course functional programming scala
        //TODO
        
        
        println ("all tests passed ...")
    }
}