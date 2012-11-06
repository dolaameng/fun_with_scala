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
sealed trait Tree[+T] {
    def isSymmetric: Boolean = this match {
        case End => true
        case Node(_, left, right) => left.isMirror(right) 
    }
    def isMirror[U](that: Tree[U]): Boolean = (this, that) match {
        case (End, End) => true
        case (Node(_, left1, right1), Node(_, left2, right2)) => 
                left1.isMirror(right2) && right1.isMirror(left2)
        case _ => false
    }
    // The >: T is because addValue must be contravariant to T
    // whereas the Tree class is covariant to T, so another type
    // variable U must be defined taking T as the lower bound.
    // Conceptually, we are adding nodes above existing nodes. 
    // In order for the subnodes to be of type T or any subtype,
    // the upper nodes must be of type T or any supertype.
    // The <% Ordered[U] allows us to use the < operator on the values
    // in the tree.
    def addValue[U >: T <% Ordered[U]](x: U): Tree[U] = this match {
        case End => Node(x, End, End)
        case Node(value, left, right) if x <= value => Node(value, left.addValue(x), right)
        case Node(value, left, right) if x > value => Node(value, left, right.addValue(x))
        case _ => throw new Exception("should not match here")
    }
}
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
// singleton Node serves a factory method - creating leaf nodes
object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
}
// singleton Tree serve sevearl factory methods
object Tree {
    // construct all possible completely balanced trees
    def cBalanced[T](n: Int, value: T) : Set[Tree[T]] = n match {
        case 0 => Set(End)
        //case 1 => List(Node(value))
        case n if n % 2 == 1 => {
            val subtrees = cBalanced( (n-1) / 2, value)
            //subtrees map (t => Node(value, t, t))
            for {
                t1 <- subtrees
                t2 <- subtrees
            } yield Node(value, t1, t2)
        }
        case n if n % 2 == 0 => {
            val sub1 = cBalanced( (n-1) / 2, value)
            val sub2 = cBalanced( (n-1) / 2 + 1, value)
            (for {
                t1 <- sub1
                t2 <- sub2
            } yield (t1, t2)) flatMap {
                case (t1,t2) => List(Node(value, t1, t2), Node(value, t2, t1))
            }
        }
    }
    // construct a binary search tree from a list of elements
    def fromList[T <% Ordered[T]](xs: List[T]) : Tree[T] = {
        xs.foldLeft(End: Tree[T]) { (tree, x) =>
            tree.addValue(x)
        }
    }
    // generate all symmetric and completely balanced trees
    def symmetricBalancedTrees[T](n: Int, value: T) : Set[Tree[T]] = {
        cBalanced(n, value) filter (_.isSymmetric)
    }
}

object BinaryTree {
    // main entry for tests
    def main(args: Array[String]) {
        // test construct a tree
        val tree = Node('a',
                        Node('b', Node('d'), Node('e')),
                        Node('c', End, Node('f', Node('g'), End)))
        // P54 - Omitted
        
        // P55 Construct completely balanced binary trees
        // a balanced tree is a binary tree with the following property:
        // the number of nodes in its left subtree and the number of nodes
        // in its right sutree are almost equal (the difference is not greater than 1)
        // Define another factory method in Tree singleton, write a function Tree.cBalanced 
        // to construct a completely balanced binary trees for a gien number of nodes.
        // The function should take as parameters the number of nodes and a single value to 
        // put in all of them.
        assert (Tree.cBalanced(0, 'x) == Set(End))
        assert (Tree.cBalanced(1, 'x) == Set(Node('x)))
        assert (Tree.cBalanced(2, 'x) == Set(Node('x, Node('x), End), Node('x, End, Node('x))))
        assert (Tree.cBalanced(3, 'x) == Set(Node('x, Node('x), Node('x))))
        assert (Tree.cBalanced(4, 'x) == 
            Set(Node('x, Node('x, End, End), 
                Node('x, End, Node('x, End, End))), 
                Node('x, Node('x, End, Node('x, End, End)), Node('x, End, End)), 
                Node('x, Node('x, End, End), Node('x, Node('x, End, End), End)), 
                Node('x, Node('x, Node('x, End, End), End), Node('x, End, End))))
                
        // P56 Symmetric binary trees
        // A binary tree is symmetric if the right subtree of the root is the mirror
        // image of the left subtree.
        // Add isSymmetric method to the Tree class to check if a given binary tree
        // is symmetric. For symmetrics, we are only interested in the structure, not 
        // in the contents of the nodes.
        assert (Node('a', Node('b'), Node('c')).isMirror(Node('a', Node('b'), Node('c'))))
        assert (Node('a', Node('b'), End).isMirror(Node('a', End, Node('b'))))
        assert (!End.isMirror(Node('a')))
        assert (Node('a').isMirror(Node('a', End, End)))
        assert (Node('a', Node('b'), Node('c')).isSymmetric)
        assert (! Node('a', End, Node('b')).isSymmetric)
        assert (Node('r', Node('a', Node('b'), End), Node('a', End, Node('b'))).isSymmetric)
        
        // P57 Binary search trees (dictionaries)
        // write a function to add an element to a binary search tree
        var searchTree: Tree[Int] = End
        searchTree = searchTree.addValue(2)
        assert (searchTree == Node(2, End, End))
        searchTree = searchTree.addValue[Int](3)
        assert (searchTree == Node(2, End, Node(3)))
        searchTree = searchTree.addValue[Int](0)
        assert (searchTree == Node(2, Node(0), Node(3)))
        // write a function to construct a binary search tree
        // from a list of elements (integers)
        assert (Tree.fromList(List(3, 2, 5, 7, 1))
            == Node(3, 
                    Node(2, Node(1), End),
                    Node(5, End, Node(7))))
        // test if the constructed binary search tress is symmetric or not
        assert (Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric)
        assert (!Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric)
        
        // P58 generate and test the paradigm
        // apply the generate and test paradigm to construct all symmetric,
        // completely balanced binary trees with a given number of nodes.
        assert (Tree.symmetricBalancedTrees(5, 'x')
            == Set(Node('x', Node('x', End, Node('x')), Node('x', Node('x'), End)), 
                   Node('x', Node('x', Node('x'), End), Node('x', End, Node('x')))))
                   
        // P59 construct height balanced binary trees
        // A height balanced binary tree: for every node, the height of its left 
        // the height of its right are almost equal (diff within one)
        // Write a method called Tree.hbalTrees to construct height-balanced
        // binary trees for a given height with a supplied value for the nodes.
        // The function should generate all solutions
        println (Tree.hbalTrees(3, 'x'))
        
        print ("all tests passed ...")
    }
}