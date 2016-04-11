package qa.paths

import scala.collection.mutable._

class Path {
  val elems = Stack[(Node, Option[(String, Double)])]()

  override def toString = elems.reverse.map(_._1).mkString(" -> ")

  def push(node: Node, edgeInfo: Option[(String, Double)]) = { 
    elems.push((node, edgeInfo))
    this 
  }

  def pop() = { 
    elems.pop
    this 
  }

  def copy(those: Stack[(Node, Option[(String, Double)])]) = { 
    those.reverse.foreach(n => push(n._1, n._2))
    this 
  }
  
  def top() = if(!elems.isEmpty) Some(elems.top._1) else None
}
