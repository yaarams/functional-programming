package functional.part1

object maybeMonad {

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
    // def map[B](f: A => B): Maybe[B] = this match {
    //   case Just(x) => Just(f(x))
    //   case None => None
    // }
    
    // alternative implementation with pure and flatMap (a more minimal set of basic functions)
    def map[B](f: A => B): Maybe[B] = this.flatMap(a => Maybe.pure(f(a)))

    // monad interface
    def flatMap[B](f: A => Maybe[B]): Maybe[B] = this match {
      case Just(x) => f(x)
      case None => None
    }

    // alternative implementation (using flatten)
    // def flatMap[B](f: A => Maybe[B]): Maybe[B] = Maybe.flatten(this.map(f)) - can't use it if we define map in terms of flatMap
    
    // applicative (only called ap because in scally apply has a special meaning)
//    def ap[B](ff: Maybe[A => B]): Maybe[B] = (this, ff) match {
//      case (Just(x), Just(f)) => Just(f(x))
//      case _ => None
//    }


    // ap implementation in terms of flatMap (this means flatMap is a stronger abstraction)
     def ap[B](ff: Maybe[A => B]): Maybe[B] = ff.flatMap(f => this.map(f))
  }

  case class Just[+A](get: A) extends Maybe[A]

  case object None extends Maybe[Nothing]

  object Maybe {

    inline def pure[A](v: A): Maybe[A] = Just(v)
    inline def nothing(): Maybe[Nothing] = None

    def flatten[A](m: Maybe[Maybe[A]]): Maybe[A] = m match {
      case Just(x) => x
      case None => None
    }

    def lift[A, B](f: A => B): Maybe[A] => Maybe[B] = (m) => m.map(f)
    
    def map2[A, B, C](a: Maybe[A], b: Maybe[B], f: (A, B) => C): Maybe[C] = b.ap(a.map(f.curried))

    def map3[A, B, C, D](a: Maybe[A], b: Maybe[B], c: Maybe[C], f: (A, B, C) => D): Maybe[D] = c.ap(b.ap(a.map(f.curried)))
  }

}