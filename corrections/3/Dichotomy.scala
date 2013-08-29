package exo 

object Dichotomy extends App {

  def find( n: Int, ary: Array[Int] ): Int = {
    
    def findRec( from: Int, to: Int ): Int = 
      if( to - from <= 1 ) {
	if( ary(from) == n ) from
	else if( ary(to) == n ) to
	else -1
      } else {
	val mid = (from+to)/2
	if( ary(mid) == n ) mid
	else if( n > ary(mid)  ) findRec(  mid, to )
	else findRec( from, mid )
      } 
    
    if(n < ary(0) || n > ary(ary.size-1) ) -1
    else findRec( 0, ary.size-1 )
  }
  

  val ary = Array(1,3,4,6,7,8,11,14,21,24,25,30)
  
  println( find( 12, ary ) )
  println( find( 11, ary ) )
  println
  println( find( 100, ary ) )
  println( find( -1, ary ) )
  println
  println( find( 1, ary ) )
  println( find( 30, ary ) )
  
}
