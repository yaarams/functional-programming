package functional.part1

object maybeApplicative {

  trait Maybe[+A] {

    def getOrElse[B >: A](default: B): B = this match {
      case Just(x) => x
      case None => default
    }

    def isDefined: Boolean = this match {
      case Just(x) => true
      case None => false
    }

    // functor
    def map[B](f: A => B): Maybe[B] = this match {
      case Just(x) => Just(f(x))
      case None => None
    }
    
    // applicative (only called ap because in scally apply has a special meaning)
    def ap[B](ff: Maybe[A => B]): Maybe[B] = this match {
      case Just(x) => ff match {
        case Just(f) => Just(f(x))
        case None => None
      }
      case None => None
    }

  }

  case class Just[+A](get: A) extends Maybe[A]

  case object None extends Maybe[Nothing]

  object Maybe {

    inline def pure[A](v: A): Maybe[A] = Just(v)
    inline def nothing(): Maybe[Nothing] = None

    def lift[A, B](f: A => B): Maybe[A] => Maybe[B] = (m) => m.map(f)
    
    def map2[A, B, C](a: Maybe[A], b: Maybe[B], f: (A, B) => C): Maybe[C] = b.ap(a.map(f.curried))

    def map3[A, B, C, D](a: Maybe[A], b: Maybe[B], c: Maybe[C], f: (A, B, C) => D): Maybe[D] = c.ap(b.ap(a.map(f.curried)))
  }

}