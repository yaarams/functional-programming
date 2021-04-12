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
  given sumInt: Monoid[Int] with {
    def empty = 0

    def combine(x: Int, y: Int) = x + y
  }
  
  // lets give this definition the name product
  given productInt: Monoid[Int] with {
    def empty = 1

    def combine(x: Int, y: Int) = x * y
  }
  
  given stringMonoid: Monoid[String] with {
    def empty = ""

    def combine(x: String, y: String): String = x.concat(y)
  }

  given listMonoid[A]: Monoid[List[A]] with {
    def empty = List.empty

    override def combine(x: List[A], y: List[A]): List[A] = x.concat(y)
  }

  given funcMonoid[A]: Monoid[A => A] with{
    override def empty: A => A = identity
    
    override def combine(x: A => A, y: A => A): A => A = x andThen y
  }
  
  given tupleMonoid[A: Monoid, B: Monoid]: Monoid[(A, B)] with {
    override def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)

    override def combine(x: (A, B), y: (A, B)): (A, B) = (Monoid[A].combine(x._1, y._1), Monoid[B].combine(x._2, y._2))
  }
  
  given mapMonoid[K, V: Monoid]: Monoid[Map[K, V]] with {
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
