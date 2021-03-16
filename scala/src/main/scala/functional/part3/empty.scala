package functional.part3

object empty {

  trait Empty[A] {
    def empty: A
  }

  object Empty {
    inline def apply[A: Empty]: Empty[A] = summon[Empty[A]]
  }
  
  given Empty[Int] {
    override def empty: Int = 0
  }

  given Empty[String] {
    override def empty: String = ""
  }

  given[A] as Empty[List[A]] {
    def empty = List.empty
  }

  given[A] as Empty[A => A] {
    override def empty: A => A = identity
  }

  // We can define it for any map, not just maps where values have some constraint
  given[K, V] as Empty[Map[K, V]] {
    override def empty: Map[K, V] = Map.empty
  }
  
  given[A: Empty, B: Empty] as Empty[(A, B)] {
    override def empty: (A, B) = (Empty[A].empty, Empty[B].empty)
  }

}
