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
  
  // NEW ADDITION
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
  // - Implement function that inserts node into tree
  // - Tree has to be rebalanced after inserting : recoloring + rotation
  // - Implement function that deletes node
  // - Add another function that takes a root or a subtree and return a node that breaks the RB-tree invariant or None if the root/subtree is a RB-tree

}
