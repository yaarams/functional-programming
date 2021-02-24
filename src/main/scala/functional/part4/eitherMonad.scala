package functional.part4

import functional.part3.monad.Monad

object eitherMonad {
  
  given[E, A] as Monad[[X] =>> Either[E, X]] {

    override def pure[A](a: A): Either[E, A] = Right(a)
    
    override def flatMap[A, B](ma: Either[E, A], f: A => Either[E, B]): Either[E, B] = ma.flatMap(f)

  }
  
  
}
