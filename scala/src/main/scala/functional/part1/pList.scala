package functional.part1

object pList {

  sealed trait PList[+A] {

    def map[B](f: A => B): PList[B]

    override def toString(): String = this match {
      case PCons(h, t) => f"${h}:${t.toString}"
      case PNil => "Nil"
    }
  }

  case class PCons[+A](head: A, tail: PList[A]) extends PList[A] {
    override def map[B](f: A => B): PList[B] = PCons(f(head), tail.map(f))
  }

  // notice Nil is an object, we don't need a whole class, it's always a single value
  case object PNil extends PList[Nothing] {
    override def map[B](f: Nothing => B): PList[B] = PNil
  }

  // companion object (similar to java statics)
  object PList {
    // destructuring of lambda values requires this structure {case (x, y) => ...}
    // apply is a special method in scala similar to python __call__ (convenient way to define builders)
    def apply[A](rest:A*): PList[A] = rest.reverse.foldLeft[PList[A]](PNil){ case (acc, v) => PCons(v, acc) }

  }

}

