package functional.part4

import functional.part3.monad.Monad

object eitherMonad {
  
  given [E]: Monad[[X] =>> Either[E, X]] with{

    def pure[A](a: A): Either[E, A] = Right(a)
    
    def flatMap[A, B](ma: Either[E, A], f: A => Either[E, B]): Either[E, B] = ma.flatMap(f)

  }
  
 
  // some examples - it doesn't have to be here but it also can't be placed in a worksheet (current limitation)

  case class ArithmeticError(msg: String)

  // as with other types with multiple arguments, it's worth creating aliases for the context they
  // will be used in
  type Arithmetic[A] =  Either[ArithmeticError, A]
  def fail(msg: String) = Left(ArithmeticError(msg))
  def success[A](a: A) = Right(a)
  def pure[A](a: A) = Monad[Arithmetic].pure(a)
  
}
