package exo

object Euclid extends App {

  def gcd( i: Int, j: Int ): Int = {
    var a = i.max(j)
    var b = i.min(j)
    while( b != 0 ) {
      val t = b
      b = a % t
      a = t
    }
    a
  }

  println( gcd( 100, 30 ) )
  println( gcd( 30, 100 ) )
  println( gcd( 12, 3732 ) )
  println( gcd( 1, 3732 ) )
  println( gcd( 25, 3732 ) )

}
