// ICCS331 Project : Red Black Tree Scala Implementation
// *****************************************************
// Naran Kongpithaksilp (Tan) - #6180233

object RedBlackTree extends App {

  sealed abstract class Color
    case class Red() extends Color
    case class Black() extends Color


  sealed abstract class Tree
    case class Empty() extends Tree
    case class Node(left: Tree, right: Tree, key: Int, color: Color)


  def size(t: Tree) : Int = t match {
    case Empty() => 0
    case Node(left,right,key,color) => size(left) + 1 + size(right)
  }

  def isBlack(t: Tree): Boolean = t match {
    case Empty() => true
    case Node(Black(),_,_,_) => true
    case _ => false
  }
  
  // To-do List:
  // - Implement function that inserts node into tree
  // - Tree has to be rebalanced after inserting : recoloring + rotation
  // - Implement function that deletes node
  // - Don't know what else 

}
