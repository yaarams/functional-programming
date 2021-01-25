package functional.part3

object monoidDisambiguationByNaming {

  trait Monoid[A] {
    def empty: A

    def combine(x: A, y: A): A
  }

  object Monoid {
    inline def apply[A: Monoid]: Monoid[A] = summon[Monoid[A]]
  }

  // lets give this definition the name sum
  given sum as Monoid[Int] with {
    def empty = 0

    def combine(x: Int, y: Int) = x + y
  }
  
  // lets give this definition the name product
  given product as Monoid[Int] with {
    def empty = 1

    def combine(x: Int, y: Int) = x * y
  }
  
  given Monoid[String] {
    def empty = ""

    override def combine(x: String, y: String): String = x.concat(y)
  }

  given[A] as Monoid[List[A]] {
    def empty = List.empty

    override def combine(x: List[A], y: List[A]): List[A] = x.concat(y)
  }

  given[A] as Monoid[A => A] {
    override def empty: A => A = identity
    
    override def combine(x: A => A, y: A => A): A => A = x andThen y
  }
  
  given[A: Monoid, B: Monoid] as Monoid[(A, B)] {
    override def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)

    override def combine(x: (A, B), y: (A, B)): (A, B) = (Monoid[A].combine(x._1, y._1), Monoid[B].combine(x._2, y._2))
  }
  
  given[K, V: Monoid] as Monoid[Map[K, V]] {
    override def empty: Map[K, V] = Map.empty

    override def combine(x: Map[K, V], y: Map[K, V]): Map[K, V] = {
      val combinedMap = x.toList.concat(y.toList).groupMap { case (k, _) => k }(_._2)
      combinedMap.view.mapValues {
        case List(a) => a
        case List(a, b) => Monoid[V].combine(a, b)
        case _ => throw AssertionError("Ha?")
      }.toMap
    }
  }

  // To all monoids add |+| operator as combine
  extension[A: Monoid](x: A) {
    def |+|(y: A): A = Monoid[A].combine(x, y)
  }

  // Add combineAll to lists if elements form a monoid
  extension[A: Monoid](lst: List[A]) {
    def combineAll = lst.foldRight(Monoid[A].empty)(Monoid[A].combine)
  }
  
}
