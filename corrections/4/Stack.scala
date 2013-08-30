package exo

import java.util.ArrayList

class Stack[A]{

  private val lst = new ArrayList[A]()

  def isEmpty = lst.isEmpty
  def size = lst.size

  def push( a: A ): Unit = lst.add(a)

  def pop(): A = {
    val last = size - 1
    val a = lst.get( last )
    lst.remove( last )
    a
  }

  def swap() = {
    if( size >= 2 ) {
      val a = pop()
      val b = pop()
      push(a)
      push(b)
    }
  }

  def map[B]( f: A=>B ): Stack[B] = {
    val stackB = new Stack[B]()
    var i = size-1
    while( i >= 0 ) {
      stackB.push( f( lst.get(i) ) )
      i -= 1
    }
    stackB
  }

}
