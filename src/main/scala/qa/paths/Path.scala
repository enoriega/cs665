package qa.paths

import scala.collection.mutable._

class Path {
  val nodes = Stack[Node]()
  override def toString = nodes.reverse.mkString(" -> ")

  def push(node: Node) = { nodes.push(node); this }
  def pop() = { nodes.pop(); this }
  def copy(those: Stack[Node]) = { those.reverse.foreach(n => push(n)); this }
}
