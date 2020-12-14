package functional.part1

object maybePart3 {

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

    // functor
    def map[B](f: A => B): Maybe[B] = Maybe.map(this, f)
    
    // applicative (only called ap because in scalla apply has a special meaning)
    def ap[B](ff: Maybe[A => B]): Maybe[B] = Maybe.ap(this, ff)

  }

  case class Just[+A](v: A) extends Maybe[A]

  case object None extends Maybe[Nothing]

  object Maybe {

    inline def pure[A](v: A): Maybe[A] = Just(v)
    inline def nothing(): Maybe[Nothing] = None

    def map[A, B](a: Maybe[A], f: A => B): Maybe[B] = a match {
      case Just(x) => Just(f(x))
      case None => None
    }

    def lift[A, B](f: A => B): Maybe[A] => Maybe[B] = (m) => map(m, f)


    def ap[A, B](ma: Maybe[A], ff: Maybe[A => B]): Maybe[B] = (ma, ff) match {
      case (Just(x), Just(f)) => Just(f(x))
      case _ => None
    }
    
    def map2[A, B, C](a: Maybe[A], b: Maybe[B], f: (A, B) => C): Maybe[C] = ap(b, map(a, f.curried))

    def lift2[A, B, C](f: (A, B) => C): (Maybe[A], Maybe[B]) => Maybe[C] = (a, b) => map2(a, b, f)

    def map3[A, B, C, D](a: Maybe[A], b: Maybe[B], c: Maybe[C], f: (A, B, C) => D): Maybe[D] = ap(c, ap(b, map(a, f.curried)))
    
    def lift3[A, B, C, D](f: (A, B, C) => D): (Maybe[A], Maybe[B], Maybe[C]) => Maybe[D] = (a, b, c) => map3(a, b, c, f)
   
    // Can define mapN+1 
  }

}