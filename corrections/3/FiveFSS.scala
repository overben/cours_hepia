package exo

import java.util.Calendar
import java.util.GregorianCalendar
 
object FiveFSS extends App {

  import Calendar.{DAY_OF_WEEK,FRIDAY,MONTH}
  
  private val years = Array.ofDim[Boolean](201)
  private val month31 = {
    import Calendar._
    Array( JANUARY, MARCH, MAY, JULY, AUGUST, OCTOBER, DECEMBER ) 
  }

  val months = new StringBuilder
  
  private def eachMonth( m: Int, year: Int, nMonths: Int ): Int =
    if( m < month31.size ) {
      val month = month31(m)
      val date = new GregorianCalendar( year, month, 1 )
      val n = if( date.get( DAY_OF_WEEK ) == FRIDAY ) {
	years( year - 1900 ) = true
	months.append( ( date.get(MONTH) + 1 ) + "-" + year + "\n" )
	1
      } else 0
      eachMonth( m+1, year, nMonths + n )
    } else nMonths
  

  private def eachYear( year: Int, nMonths: Int ): Int = 
    if( year <= 2100 ) {
      val n = eachMonth( 0, year, nMonths )
      eachYear( year + 1, n )
    } else nMonths

  val numMonths = eachYear( 1900, 0 )

  println("There are "+numMonths+" months with five weekends from 1900 through 2100:")
  println(months)
  println("Years with no five-weekend months:")
  


}
