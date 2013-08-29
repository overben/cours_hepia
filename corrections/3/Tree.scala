package exo

trait Tree[A] {
  def size: Int = this match {
    case Empty() => 0
    case Leaf(_) => 1
    case Node( l, r ) => l.size + r.size
  }
  def depth: Int = this match {
    case Empty() => 0
    case Leaf(_) => 1
    case Node( l, r ) => 1 + ( l.depth max r.depth )
  }
}

case class Leaf[A]( a: A ) extends Tree[A]
case class Empty[A]() extends Tree[A]
case class Node[A]( left: Tree[A], right: Tree[A] ) extends Tree[A]

object TreeDemo extends App {
  
  val tree = Node(
    Node(
      Node(
	Leaf(1),
	Leaf(2)
      ),
      Node(
	Empty[Int](),
	Leaf(3)
      )
    ),
    Node(
      Leaf(4),
      Empty[Int]()
    )
  )

  println( tree )
  println( tree.size )
  println( tree.depth )
  


}
