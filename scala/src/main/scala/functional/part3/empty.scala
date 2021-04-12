package functional.part3

object empty {

  trait Empty[A] {
    def empty: A
  }

  object Empty {
    inline def apply[A: Empty]: Empty[A] = summon[Empty[A]]
  }
  
  given Empty[Int] with {
    def empty: Int = 0
  }

  given Empty[String] with {
    def empty: String = ""
  }

  given [A]:Empty[List[A]] with{
    def empty = List.empty
  }

  given [A]:Empty[A => A] with {
    def empty: A => A = identity
  }

  // We can define it for any map, not just maps where values have some constraint
  given [K, V]:Empty[Map[K, V]] with {
    override def empty: Map[K, V] = Map.empty
  }
  
  given [A: Empty, B: Empty]:Empty[(A, B)] with {
    override def empty: (A, B) = (Empty[A].empty, Empty[B].empty)
  }

}
