package exo

case class RNG( i: Int ) {

  lazy val nextInt = {
    val next = ( 16807 * i ) % ( 2 << 30 - 1 )
    ( next, RNG(next) )
  }

  def nextIntList( n: Int ): (List[Int],RNG) = {
    def build( lst: List[Int], rng: RNG ): (List[Int],RNG) = 
      if( lst.size == n ) (lst,rng)
      else {
	val (next,rng2) = rng.nextInt
	build( next::lst, rng2 )
      }
    build(Nil,this) 
  }

}

trait Gen[+A] { self =>

  def next: (A,Gen[A])

  def nextList( n: Int ): (List[A],Gen[A]) = {    
    def build( lst: List[A], gen: Gen[A] ): (List[A],Gen[A]) = 
      if( lst.size == n ) (lst,gen)
      else {
	val (next,gen2) = gen.next
	build( next::lst, gen2 )
      }
    build(Nil,this) 
  }

  def map[B]( f: A=>B ): Gen[B] = new Gen[B] {
    lazy val next = {
      val (a,genA) = self.next
      ( f(a), genA.map(f) )
    }
  }

}

object Gen {

  case class GenInt( seed: Int ) extends Gen[Int] {
    private lazy val nextInt = ( 16807 * seed ) % ( 2 << 30 - 1 )    
    lazy val next = ( nextInt, GenInt(nextInt) )
  }

  def ofInt( seed: Int ) = GenInt(seed)

  def ofDouble( seed: Int ) = ofInt( seed ).map { i=>
    val range = Int.MaxValue.toDouble - Int.MinValue.toDouble
    i / range + Int.MinValue
  }

  def ofBoolean( seed: Int ) = ofDouble( seed ).map( _ >= 0.5 )


}
