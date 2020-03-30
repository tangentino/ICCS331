// ICCS331 Project : Red Black Tree Scala Implementation
// *****************************************************
// Naran Kongpithaksilp (Tan) - #6180233
// *****************************************************
// RBTree Properties:
// 1) Every node has a color either red or black.
// 2) Root of tree is always black.
// 3) There are no two adjacent red nodes (A red node cannot have a red parent or red child).
// 4) Every path from a node (including root) to any of its descendant NULL node has the same number of black nodes.
// *****************************************************

object RedBlackTree extends App {

  sealed abstract class Color
  // Color of tree
  case class Red() extends Color
  case class Black() extends Color


  sealed abstract class Tree
  // Tree class
  case class Empty() extends Tree
  case class Node(color: Color, left: Tree, right: Tree, key: Int) extends Tree


  def size(t: Tree) : Int = t match {
    // Returns total number of nodes
    case Empty() => 0
    case Node(color,left,right,key) => size(left) + 1 + size(right)
  }

  def isBlack(t: Tree): Boolean = t match {
    // Returns boolean saying if tree is black
    case Empty() => true
    case Node(Black(),left,right,key) => true
    case _ => false
  }

  // NEW ADDITIONS
  def makeTree(lst: List[Int]): Tree = {
    // Takes a list of Int and makes a RB tree consisting of all the values in the list
    def makeTreeHelper(lst: List[Int], ans: Tree): Tree = lst match {
      case Nil => ans
      case head::tail => makeTreeHelper(tail,insert(head,ans))
    }
    makeTreeHelper(lst,Empty())
  }

  def blackNodesInPath(t: Tree): Int = t match {
    // Returns number of black nodes on left path of t
    case Empty() => 0
    case Node(Black(),l,r,k) => blackNodesInPath(l) + 1
    case Node(Red(),l,r,k) => blackNodesInPath(l)
  }

  def isBalanced(t: Tree): Boolean = t match {
    // Return boolean saying if the RB Tree is balanced properly
    case Empty() => true
    case Node(c,l,r,k) =>
      // Tree is balanced if every path has the same number of black nodes
      // isBalanced makes sure that the function checks every node (returns true when it reaches leaf)
      // Then makes sure that number of black nodes in each path is the same
      isBalanced(l) && isBalanced(r) && blackNodesInPath(l) == blackNodesInPath(r)
  }

  def height(t: Tree): Int = t match {
    // Returns number of nodes in the longest path
    case Empty() => 0
    case Node(c,l,r,k) => math.max(height(l),height(r)) + 1

  }

  def traverse(t: Tree): Any = {
    // Traversal of tree for testing purposes
    t match {
      case Empty() => None
      case Node(c,l,r,k) =>
        traverse(l)
        print("%d ".format(k))
        traverse(r)
    }
  }

  def turnBlack(t: Tree): Tree = {
    // Turn a tree black
    t match {
      case Node(Red(),l,r,k) => Node(Black(),l,r,k)
      case _ => t
    }
  }

  def insert(x: Int,t: Tree): Tree = {
    // Add a value to a tree
    def insertHelper(key: Int, tree: Tree): Tree = {
      tree match {
        case Empty() =>
          // If tree is empty then just add the node
          Node(Red(), Empty(), Empty(), key)
        case Node(c, l, r, k) =>
          if (key < k)
            // It's a binary tree so if the value is small then add it to the left
            balance(c,insertHelper(key,l),r,k)
          else if (key > k)
            // If value is bigger than add to the right
            balance(c,l,insertHelper(key,r),k)
          else
            // Else: do nothing
            t
      }
    }
    // Root must be black so we blacken the tree every time we add a new value
    turnBlack(insertHelper(x,t))
  }

  def balance(c: Color, a: Tree, b: Tree, k: Int): Tree = {
    // Takes unbalanced tree and balances to Black(k1) <--- Red(k2) ---> Black(k3)
    Node(c,a,b,k) match {
      case Node(Black(),Node(Red(),Node(Red(),a,b,k1),c,k2),d,k3) =>
        // Left left Case: Rotate right and balance
        Node(Red(),Node(Black(),a,b,k1),Node(Black(),c,d,k3),k2)

      case Node(Black(),Node(Red(),a,Node(Red(),b,c,k2),k1),d,k3) =>
        // Left right Case: Rotate left and apply left left case and balance
        Node(Red(),Node(Black(),a,b,k1),Node(Black(),c,d,k3),k2)

      case Node(Black(),a,Node(Red(),b,Node(Red(),c,d,k3),k2),k1) =>
        // Right right Case: Rotate left and balance
        Node(Red(),Node(Black(),a,b,k1),Node(Black(),c,d,k3),k2)

      case Node(Black(),a,Node(Red(),Node(Red(),b,c,k2),d,k3),k1) =>
        // Right left Case: Rotate right and apply right right case and balance
        Node(Red(),Node(Black(),a,b,k1),Node(Black(),c,d,k3),k2)

      case _ =>
        // No balancing needed
        Node(c,a,b,k)
    }
  }

  // TEST CASES
  // ***********************
  
  // isBlack() and turnBlack() Tests
  val testRed = Node(Red(),testBlack,Empty(),5)
  val testBlack = Node(Black(),Empty(),Empty(),2)
  assert(isBlack(testRed) == false)
  assert(isBlack(testBlack) == true)
  assert(isBlack(turnBlack(testRed)) == true)

  // Test if Red Black Tree actually works
  val t1 = insert(4,insert(2,insert(1,insert(5,Node(Black(),Empty(),Empty(),3)))))
  assert(size(t1) == 5)
  assert(height(t1) == 3)
  assert(isBlack(t1) == true)
  println(traverse(t1)) // Should print 1 2 3 4 5 and None (leaf)
  assert(isBalanced(t1) == true)

  // Test if it works when inserted out of order
  val t2 = insert(2,insert(5,insert(3,insert(4,insert(1,Empty())))))
  assert(size(t2) == 5)
  assert(height(t2) == 3)
  assert(isBlack(t2) == true)
  println(traverse(t2)) // Should print 1 2 3 4 5 None
  assert(isBalanced(t2) == true)

  // More tests
  val t3 = insert(13,insert(8,insert(1,insert(6,insert(11,insert(17,insert(27,insert(22,insert(25,insert(15,Empty()))))))))))
  assert(size(t3) == 10)
  assert(height(t3) == 4)
  assert(isBlack(t3) == true)
  println(traverse(t3)) // Should print 1 6 8 11 13 15 17 22 25 27 None
  assert(isBalanced(t3) == true)

  val t4 = insert(10,insert(7,insert(3,insert(12,insert(17,insert(14,insert(26,insert(38,insert(30,insert(41,insert(47,insert(39,insert(35,insert(36,insert(28,insert(15,insert(16,insert(21,insert(23,insert(19,insert(20,Empty())))))))))))))))))))))
  assert(size(t4) == 21)
  assert(height(t4) == 6)
  assert(isBlack(t4) == true)
  println(traverse(t4)) // Should print 3 7 10 12 14 15 16 17 19 20 21 23 26 28 30 35 36 38 39 41 47 None
  assert(isBalanced(t4) == true)

  val t5 = makeTree(List(10,7,3,15,12,14,20,19,26,28,35,41,39,47,16,30,23,21,38,36,17))
  assert(size(t5) == 21)
  assert(height(t5) == 6)
  assert(isBlack(t5) == true)
  println(traverse(t5)) // Should print 3 7 10 12 14 15 16 17 19 20 21 23 26 28 30 35 36 38 39 41 47 None
  assert(isBalanced(t5) == true)

  val t6 = makeTree(List(1,11,15,13,6,8,27,17,25,22))
  assert(size(t6) == 10)
  assert(height(t6) == 4)
  assert(isBlack(t6) == true)
  println(traverse(t6)) // Should print 1 6 8 11 13 15 17 22 25 27 None
  assert(isBalanced(t6) == true)

  val t7 = makeTree(List(5,3,4,1,2))
  assert(size(t7) == 5)
  assert(height(t7) == 3)
  assert(isBlack(t7) == true)
  println(traverse(t7)) // Should print 1 2 3 4 5 and None (leaf)
  assert(isBalanced(t7) == true)

}

