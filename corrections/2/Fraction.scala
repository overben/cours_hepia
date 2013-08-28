package exo

case class Fraction( num: Int, den: Int ) extends Ordered[Fraction] {

  lazy val simplify = {
    val x = Euclid.gcd(num,den)
    Fraction( num/x, den/x )
  }

  lazy val invert = new Fraction( den, num )
  
  override
  def equals( that: Any ): Boolean = that match {
    case f: Fraction => 
      f.simplify.num == this.simplify.num && 
      f.simplify.den == this.simplify.den
    case _ => false
  }

  override def toString = num + "/" + den

  def +( that: Fraction ) = {
    val newDen = (that.den * this.den ) / Euclid.gcd(this.den,that.den)
    val newNum = 
      this.num * newDen / this.den + that.num * newDen / that.den
    Fraction( newNum, newDen )
  }

  def -( that: Fraction ) = this + (new Fraction(-that.num,that.den))

  def *( that: Fraction ) = Fraction(
    this.num * that.num,
    this.den * that.den
  )

  def /( that: Fraction ) = this * that.invert
  
  lazy val toDouble = num.toDouble / den

  def compare( that: Fraction ) = 
    if( this.toDouble > that.toDouble ) 1
    else if( this.toDouble < that.toDouble ) -1
    else 0

}

object Fraction {

  def apply( x: Double, prec: Int ): Fraction = {
    val mag = math.pow( 10, prec ).toInt
    Fraction( (x*mag).toInt, mag ).simplify
  }


}
