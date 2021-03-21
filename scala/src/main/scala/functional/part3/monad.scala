package functional.part3

import functional.part3.applicative.*

object monad {

  trait Monad[M[_]] extends Applicative[M] {
    
//    def pure[A](a: A): M[A]

    def flatMap[A, B](ma: M[A], f: A => M[B]): M[B]

    // ------------------------- //
    // Implement the Applicative //
    // ------------------------- //

    override def ap[A, B](fa: M[A], ff: M[A => B]): M[B] =
      flatMap(ff, f => map(fa, a => f(a)))

    // we redifine map in terms of flatMap and pure or we will have an endless rcursion
    // ap defined with map -> map of applicative difined in terms of ap
    override def map[A, B](fa: M[A], f: A => B) =
      flatMap(fa, a => pure(f(a)))
      
  }

  object Monad {
    // a sligntly nicer syntax for summon
    def apply[M[_]: Monad]: Monad[M] = summon[Monad[M]]
  }
 
  // Monad useful methods and operators
  extension[M[_]: Monad, A, B](ma: M[A]) {

    def map(f: A => B): M[B] = Monad[M].map(ma, f)
    
    def flatMap(f: A => M[B]): M[B] = Monad[M].flatMap(ma, f)
    
    def <<(mb: M[B]): M[A] = Monad[M].flatMap(ma, a => Monad[M].map(mb, _ => a))
    
    def >>(mb: M[B]): M[B] = Monad[M].flatMap(ma, _ => mb)
    
  }

  // a useful Kleisli arrow operator to compose functions that return monads
  // Sometime this operator is called a 'fish operator' because it resembles a drawing of a fish
  extension[M[_]: Monad, A, B, C](f: A => M[B]) {
    def >=>(g: B => M[C]): A => M[C] = {
      a => Monad[M].flatMap(f(a), g)
    }
  }

  // --------------- //
  // Monad instances //
  // --------------- //

  given Monad[List] with {
    def pure[A](a: A): List[A] = List(a)

    def flatMap[A, B](ma: List[A], f: A => List[B]): List[B] = ma.flatMap(f)
  }

  given Monad[Option] with {

    def pure[A](a: A): Option[A] = Some(a)

    def flatMap[A, B](ma: Option[A], f: A => Option[B]): Option[B] = ma match {
      case Some(x) => f(x)
      case None => None
    }
  }
  
  // ----------------- //
  // Monad Composition //
  // ----------------- //
  
  // THERE IS NO GENERIC WAY TO COMPOSE MOANDS
  // Some specific monads can be composed with others though - we'll talk about it in the future

}
