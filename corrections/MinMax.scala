package exo

object MinMax extends App {

  def minMax( ary: Array[Int] ): (Int,Int) = {
    def minMaxRec( i: Int, min: Int, max: Int ): (Int,Int) = {
      if( i == ary.size ) (min,max)
      else {
	if( ary(i) < min ) minMaxRec( i+1, ary(i), max)
	else if( ary(i) > max ) minMaxRec( i+1, min, ary(i) )
	else minMaxRec( i+1, min, max )
      }
    }

    minMaxRec( 1, ary(0), ary(0) )
  }

  println( minMax( Array(1,24,100, -1, 2, 1000, 0) ) )

}
