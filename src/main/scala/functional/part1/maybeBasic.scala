package functional.part1

object maybeBasic {

  trait Maybe[+A] {

    def getOrElse[B >: A](default: B): B = this match {
      case Just(x) => x
      case None => default
    }

    def get: A = this match {
      case Just(x) => x
      case None => throw IllegalStateException("Can't get none")
    }
    
    def isDefined: Boolean = this match {
      case Just(x) => true
      case None => false
    }
  }

  case class Just[+A](v: A) extends Maybe[A]

  case object None extends Maybe[Nothing]

  object Maybe {

    inline def pure[A](v: A): Maybe[A] = Just(v)
    inline def nothing(): Maybe[Nothing] = None

  }

}