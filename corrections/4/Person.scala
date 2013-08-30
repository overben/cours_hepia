package exo

case class Person( 
  name: String, 
  age: Int, 
  location: String
)

case class Friends( p1: Person, p2: Person) {

  def areFriends( q1: Person, q2: Person ): Boolean =
    ( q1 == p1 && q2 == p2 ) || ( q2 == p1 && q1 == p2 )

}

object World {

  val alice = Person( "Alice", 26, "Zagreb" )
  val bob = Person( "Bob", 17, "Zagreb" )
  val charles = Person( "Charles", 22, "Lille" )
  val diego = Person( "Diego", 24, "Madrid" )
  val edouard = Person( "Edouard", 21, "Lille" )
  val fernando = Person( "Fernando", 26, "Lille" )
  val genevieve = Person( "Geneviève", 21, "Genève" )

  val persons = List( alice, bob, charles, diego, edouard, fernando, genevieve )

  val friendship = List(
    Friends( alice, bob ),
    Friends( alice, charles ),
    Friends( alice, fernando ),
    Friends( bob, diego ),
    Friends( bob, fernando ),
    Friends( charles, edouard ),
    Friends( edouard, diego )
  )

}


object PersonDemo extends App {

  import World._
  
  //Retourne la liste des personnes habitant une ville
  def livingIn( loc: String ): List[Person] = persons.filter( _.location == loc )

  //Liste des noms des personnes vivant à Zagreb
  val inZagreb = livingIn( "Zagreb" ).map( _.name )
  println( "A Zagreb: " + inZagreb ); println

  //Age moyen des personnes vivant à Lille
  val avgAgeLille = {
    val ages = livingIn("Lille").map( _.age )
    ages.sum.toDouble / ages.size
  }
  println( "Age moyen (Lille): " + avgAgeLille ); println

  //Liste des villes où habitent les personnes. Triées par ordre
  //alphabétique, sans répétition. (utiliser 'distinct' et 'sorted')
  val cities = persons.map( _.location ).distinct.sorted
  println( "Villes: " + cities ); println
  
  //Retourne une liste de liste regroupant les noms des personnes vivant dans
  //la même ville dans les sous-listes.
  val together = cities.map( c => livingIn(c).map( _.name ) )
  println( "Habitent ensemble: " + together ); println

  //Retourne la liste des amis de 'p'
  def friendsOf( p: Person ): List[Person] = persons.filter { q=>
    friendship.filter( _.areFriends(p, q) ).nonEmpty
  }

  //Liste des noms des amis de charles
  val charlesFriends = friendsOf( charles ).map( _.name )
  println( "Ami de Charles: " + charlesFriends ); println

  //Qui a le plus d'ami ? (utiliser 'maxBy')
  val hasMoreFriends = persons.maxBy( p => friendsOf(p).size )
  println( hasMoreFriends + " à le plus d'amis." ); println

  //Les paires d'amis habitant la même ville. Retourner une liste de tuples
  //de 3 éléments: nom1, nom2, ville (utiliser 'collect')
  val sameLocationFriends = friendship.collect {
    case Friends(p,q) if p.location == q.location => (p.name,q.name,p.location)
  }
  println( "Amis habitant la même ville: " + sameLocationFriends ); println
  
}
