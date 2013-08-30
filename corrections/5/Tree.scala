package exo

import concurrent.Future
import concurrent.ExecutionContext.Implicits.global


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

  def toList: List[A] = this match {
    case Empty() => Nil
    case Leaf(a) => List(a)
    case Node( l, r ) => l.toList ++ r.toList
  }

  def map[B]( f: A=>B ):Tree[B] = this match {
    case Empty() => Empty()
    case Leaf(a) => Leaf( f(a) )
    case Node(l,r) => Node( l.map(f), r.map(f) )
  }

  def filter( f: A=>Boolean ): Tree[A] = this match {
    case Empty() => Empty()
    case Leaf(a) => if( f(a) ) Leaf(a) else Empty()
    case Node(l,r) => {
      (l.filter(f), r.filter(f)) match {
	case (Empty(),Empty()) => Empty()
	case (ll,rr) => Node(ll,rr)
      }
    }
  }

  def mapFuture[B]( f: A=>B ):Future[Tree[B]] = this match {  
    case Empty() => Future( Empty() )
    case Leaf(a) => Future( Leaf( f(a) ) )
    case Node(l,r) => {
      val lF = l.mapFuture(f)
      val rF = r.mapFuture(f)
      for {
	ll <- lF
	rr <- rF
      } yield Node(ll,rr)
    }
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
  
  tree.mapFuture( _ * 0.25 ).foreach( println )
}
