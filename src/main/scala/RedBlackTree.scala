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
    case class Node(left: Tree, right: Tree, key: Int, color: Color) extends Tree


  def size(t: Tree) : Int = t match {
    // Returns size of tree
    case Empty() => 0
    case Node(left,right,key,color) => size(left) + 1 + size(right)
  }

  def isBlack(t: Tree): Boolean = t match {
    // Returns boolean saying if tree is black
    case Empty() => true
    case Node(left,right,key,Black()) => true
    case _ => false
  }
  
  // NEW ADDITIONS = EVERYTHING BELOW THIS
  
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
  
  def balance(a: Tree, b: Tree, k: Int, c: Color): Tree = {
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
  
  // To-do List:
  // - I̶̶̶m̶̶̶p̶̶̶l̶̶̶e̶̶̶m̶̶̶e̶̶̶n̶̶̶t̶̶̶ ̶̶̶f̶̶̶u̶̶̶n̶̶̶c̶̶̶t̶̶̶i̶̶̶o̶̶̶n̶̶̶ ̶̶̶t̶̶̶h̶̶̶a̶̶̶t̶̶̶ ̶̶̶i̶̶̶n̶̶̶s̶̶̶e̶̶̶r̶̶̶t̶̶̶s̶̶̶ ̶̶̶n̶̶̶o̶̶̶d̶̶̶e̶̶̶ ̶̶̶i̶̶̶n̶̶̶t̶̶̶o̶̶̶ ̶̶̶t̶̶̶r̶̶̶e̶̶̶e̶̶̶
  // - T̶r̶e̶e̶ ̶h̶a̶s̶ ̶t̶o̶ ̶b̶e̶ ̶r̶e̶b̶a̶l̶a̶n̶c̶e̶d̶ ̶a̶f̶t̶e̶r̶ ̶i̶n̶s̶e̶r̶t̶i̶n̶g̶ ̶:̶ ̶r̶e̶c̶o̶l̶o̶r̶i̶n̶g̶ ̶+̶ ̶r̶o̶t̶a̶t̶i̶o̶n̶
  // - Implement function that deletes node
  // - Add another function that takes a root or a subtree and return a node that breaks the RB-tree invariant or None if the root/subtree is a RB-tree

}
