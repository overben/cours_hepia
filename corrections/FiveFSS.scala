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
  var numMonths = 0
  var year = 1900
  while( year <= 2100 ) {
    for( month <- month31 ) {
      val date = new GregorianCalendar( year, month, 1 )
      if( date.get( DAY_OF_WEEK ) == FRIDAY ) {
	years( year - 1900 ) = true
	numMonths += 1
	months.append( ( date.get(MONTH) + 1 ) + "-" + year + "\n" )
      }
    }
    year += 1
  }

  println("There are "+numMonths+" months with five weekends from 1900 through 2100:")
  println(months)
  println("Years with no five-weekend months:")
  
  year = 1900
  while( year <= 2100 ) {
    if( years(year - 1900) ){
      println(year)
    }
    year += 1
  }

}
