package exo

case class Complex( im: Double, re: Double ) {
  def +( that: Complex ) = Complex( im+that.im, re+that.re )
  lazy val square = Complex( im*im - re*re, 2*im*re )
  lazy val modulus = math.sqrt( im*im + re*re )
  override def toString = im +"+"+ re +"i"
}

object Julia {

  //Construit une grille carrée de nombres complexes {z}, à partir
  //de bornes (boundaries) et d'une résolution (représentant le
  //nombre de point par côté
  def grid( boundaries: (Double,Double), resolution: Int ): List[List[Complex]] = ???

  //Calcule l'appartenance à l'ensemble de julia de chaque point
  //de la grille. Pour un 'c' donné et un nombre d'itérations
  //maximales
  //Vous pouvez retourner simplement une List[List[Int]] qui
  //est bien une Seq[Seq[Int]]
  def sequential( c: Complex, grid: List[List[Complex]], maxIter: Int ): Seq[Seq[Int]] = ???


  //Idem que sequential, mais en parallélisant le code à l'aide des
  //collections parallèles
  def withCollectionParallel( c: Complex, grid: List[List[Complex]], maxIter: Int ): Seq[Seq[Int]] = ???

  //Idem que sequential, mais en parallélisant le code à l'aide des Futures.
  //La méthode Future.sequence peut vous y aider.
  def withFutures( c: Complex, grid: List[List[Complex]], maxIter: Int ): Seq[Seq[Int]] = ???


  def save( filename: String, result: Seq[Seq[Int]] ): Unit = {
    import java.io._
    val max = result.map( _.max ).max
    val x = result.head.size
    val y = result.size
    val pw = new PrintWriter( filename )
    pw.println( "P2" )
    pw.println( x + " " + y )
    pw.println( max )
    result.foreach{ row =>
      pw.println( row.mkString(" ") )
    }
    pw.close
  }

}

trait Benchmark {

  private def run[A]( n: Int, comp: () => A ): A = {
    def runRec( m: Int, a: A ): A = 
      if( m <= 0 ) a
      else {
	runRec( m-1, comp() )
      }
    runRec( n-1, comp() )
  }

  private def timer[A]( n: Int, comp: () => A ): A = {
    import compat.Platform.{currentTime=>time}
    val before = time
    val a = run( n, comp )
    val duration = (time - before).toDouble / n / 1000
    println( "         took " + duration + " seconds per iteration" )
    a
  }
  
  def bench[A]( label: String, warmup: Int, measure: Int )( computation: =>A ): A = {
    println( "Computing: " + label )
    println( "  - Warming-up (" +warmup+ " iterations)" )
    timer( warmup, ()=>computation )
    println( "  - Measuring (" +measure + " iterations)" )
    timer( measure, ()=>computation )
  }

}

object JuliaDemo extends App with Benchmark {

  val maxIter = 64
  val boundaries = ( -2.0, 2.0 )
  val resolution = 1024
  val c = Complex( 0.382, 0.147 )
  
  val grid = bench("Building grid", 25, 100 ){
    Julia.grid( boundaries, resolution )
  }
  val result1 = bench("Sequential Julia", 10, 25 ){
    Julia.sequential( c, grid, maxIter )
  }
  val result2 = bench("Julia with parallel collections", 10, 25 ){
    Julia.withParallelCollections( c, grid, maxIter )
  }
  val result3 = bench("Julia with Futures", 10, 25 ){
    Julia.withFutures( c, grid, maxIter )
  }
  bench( "Saving images", 25, 100 ){
    Julia.save( "/tmp/sequential.pgm", result1 )
    Julia.save( "/tmp/parrallelCollections.pgm", result2 )
    Julia.save( "/tmp/futures.pgm", result3 )
  }

}

  
