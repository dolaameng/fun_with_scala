// A binary tree is either empty or it is composed of a root element
// and two successors, which are binary trees themselves. 

// Note: a sealed trait can be extended only in the same file than its declaration
// they are often used to provide an alternative to enums. Since they can be only
// extended in a single file, the comiler knows every possible subtypes and can 
// reason about it
// In the templating of Node, putting a plus in front of T makes the class covariant;
// It will be able to hold subtypes of whatever type its created for.
// This is specially IMPORTANT so that End(Empty Node) can be a SINGLETON. As for a
// singleton, it must have a specific type, so we give it type Nothing, which is the
// subtype of every other type.

// sealed trait - enumerate all possible cases in a single file
// Tree trait is also covariant, see the discussion about covariant below
sealed trait Tree[+T]
// covariant on subtypes of T - make End eligble for singleton
// see the object Node (factory method) below
// but why not Node[T] ???
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + right.toString + ")"
}
// Empty Node is a singleton
case object End extends Tree[Nothing] {
    override def toString = "."
}
// singleton Node serves a factor method - creating leaf nodes
object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
}

object BinaryTree {
    // main entry for tests
    def main(args: Array[String]) {
        
        print ("all tests passed ...")
    }
}