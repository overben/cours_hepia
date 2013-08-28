package exo

import java.io.{FileReader,BufferedReader}

object PrintFile extends App {

  private def ===(): Unit =  {
    println( "=" * 60 )
  }

  private def printIt( filename: String ): Unit = {
    val bf = new BufferedReader( new FileReader(filename) )
    var line = bf.readLine
    while( line != null ) {
      println( line )
      line = bf.readLine
    }
    bf.close
  }

  printIt( "build.sbt" )

  ===

  //Implémentation recursive
  private def printItRec( filename: String ): Unit = {
    def nextLine( reader: BufferedReader ): Unit = {
      val line = reader.readLine
      if( line == null ) reader.close
      else {
	println( line )
	nextLine( reader)
      }
    }
    nextLine( new BufferedReader( new FileReader(filename) ) )
  }

  printItRec( "build.sbt" )

  ===

  //Implementation alternative utilisant la libraire Scala
  def printItScala( filename: String ) {
    io.Source.fromFile(filename).getLines.foreach(println)
  }

  printItScala( "build.sbt" )


}
