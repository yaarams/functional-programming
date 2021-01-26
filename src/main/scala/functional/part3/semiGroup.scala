package functional.part3

object semiGroup {

  trait SemiGroup[A] {
    def combine(x: A, y: A): A
  }

  object SemiGroup {
    inline def apply[A: SemiGroup]: SemiGroup[A] = summon[SemiGroup[A]]
  }

  given SemiGroup[Int] with {
    def combine(x: Int, y: Int) = x + y
  }

  given SemiGroup[String] {
    override def combine(x: String, y: String): String = x.concat(y)
  }

  given[A] as SemiGroup[List[A]] {
    override def combine(x: List[A], y: List[A]): List[A] = x.concat(y)
  }

  given[A] as SemiGroup[A => A] {
    override def combine(x: A => A, y: A => A): A => A = x andThen y
  }
  
  given[A: SemiGroup, B: SemiGroup] as SemiGroup[(A, B)] {
    override def combine(x: (A, B), y: (A, B)): (A, B) = (SemiGroup[A].combine(x._1, y._1), SemiGroup[B].combine(x._2, y._2))
  }
  
  given[K, V: SemiGroup] as SemiGroup[Map[K, V]] {
    override def combine(x: Map[K, V], y: Map[K, V]): Map[K, V] = {
      val combinedMap = x.toList.concat(y.toList).groupMap { case (k, _) => k }(_._2)
      combinedMap.view.mapValues {
        case List(a) => a
        case List(a, b) => SemiGroup[V].combine(a, b)
        case _ => throw AssertionError("Ha?")
      }.toMap
    }
  }

  // To all monoids add |+| operator as combine
  extension[A: SemiGroup](x: A) {
    def |+|(y: A): A = SemiGroup[A].combine(x, y)
  }
  
}
