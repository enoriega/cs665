package qa.paths

import scala.collection.mutable._

class Path {
  val elems = Stack[(Node, Option[(String, Double)])]()

  override def toString = {
    elems.reverse.map(_._1)
      .zip(elems.reverse.map(o => o._2 match {
        case Some(o) => o._1
        case None => ""
      })).mkString(" -> ")
  }

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

  def apply(index: Int) = try {
    Some(elems(index))
    } catch {
      case aioobe: ArrayIndexOutOfBoundsException => {
        println("Index out of bounds")
        None
      }
    }

  def update(index: Int, _that: (Node, Option[(String, Double)])) = {
    elems(index) = _that
  }

  def size() = elems.size

  def reverse() = new Path().copy(this.elems.reverse)

}
