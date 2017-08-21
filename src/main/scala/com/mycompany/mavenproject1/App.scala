package com.mycompany.mavenproject1


object App 
{
  
  
  /*A huffman code  is represented by a binary tree
   * 
   * Every leaf node represents the each charecter the code tree can encode
   * The weight of the leaf node is the frequency of the charecter associared with the node
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these leaes
  
   */
  
  
  import scala.collection.mutable.{Map, PriorityQueue}
 
  sealed abstract class Tree
  case class Node(left: Tree, right: Tree) extends Tree
  case class Leaf(c: Char) extends Tree
 
  def treeOrdering(m: Map[Tree, Int]) = new Ordering[Tree] { 
     def compare(x: Tree, y: Tree) = m(y).compare(m(x))
  }
 
  def stringMap(text: String) = text groupBy (x => Leaf(x) : Tree) mapValues (_.length)
 
  def buildNode(queue: PriorityQueue[Tree], map: Map[Tree,Int]) {
    val right = queue.dequeue
    val left = queue.dequeue
    val node = Node(left, right)
    map(node) = map(left) + map(right)
    queue.enqueue(node)
  }
 
  def codify(tree: Tree, map: Map[Tree, Int]) = {
    def recurse(tree: Tree, prefix: String): List[(Char, (Int, String))] = tree match {
      case Node(left, right) => recurse(left, prefix+"0") ::: recurse(right, prefix+"1")
      case leaf @ Leaf(c) => c -> ((map(leaf), prefix)) :: Nil
    }
    recurse(tree, "")
  }
 
  def encode(text: String) = {
    val map = Map.empty[Tree,Int] ++= stringMap(text)
    val queue = new PriorityQueue[Tree]()(treeOrdering(map)) ++= map.keysIterator
 
    while(queue.size > 1) {
      buildNode(queue, map)
    }
    codify(queue.dequeue, map)
  }
  
  
  
  
  
  
  
    def main(args: Array[String]):Unit={
    val text = "i have ejsndasl ppqwe niuwegyr  adia sdsdsd sdsdsdsds sdsdsdsdsd akdjpd spd;l dsd adn jnsnd ksndks dksmdka dsdkd ksdsm; msdl dsl msfsfd dfmkfndf kdfmdkf df  kdf kdfm mdfdf mdf dfm; dfdlf d;md;f;f d;f  dmmmsds sdsa lkjn skbfakjd s sjdnskdnalsd al lakdnalk d dfsldsld l dskdsklkl k dskdakld alkdaskld"
    val code = encode(text)
    println("Char\tWeight\t\tEncoding")
    code sortBy (_._2._1) foreach { 
      case (c, (weight, encoding)) => println("%c:\t%3d/%-3d\t\t%s" format (c, weight, text.length, encoding)) 
    }
    }
}
