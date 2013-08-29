package exo

case class RNG( i: Int ) {

  lazy val nextInt: (Int,RNG) = {
    val next = ( 16807 * i ) % ( 2 << 30 - 1 )
    ( next, RNG(next) )
  }

}

class RNGM( seed: Int ) {

  private var i = seed

  def nextInt: Int = {
    i = ( 16807 * i ) % ( 2 << 30 - 1 )
    i
  }

}
