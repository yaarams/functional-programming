package functional.part3

import functional.part3.empty.*
import functional.part3.semiGroup.*

object monoidHirarchical {

  trait Monoid[A] {
    def empty: A
    def combine(x: A, y: A): A
  }

  object Monoid {
    inline def apply[A: Monoid]: Monoid[A] = summon[Monoid[A]]
  }
 
  given [A: SemiGroup: Empty]: Monoid[A] with {
    override def empty: A = Empty[A].empty
    override def combine(x: A, y: A): A = SemiGroup[A].combine(x, y)
  }
  
  // Add combineAll to lists if elements form a monoid
  extension[A: Monoid](lst: List[A]) {
    def combineAll = lst.foldRight(Monoid[A].empty)(Monoid[A].combine)
  }
  
}
