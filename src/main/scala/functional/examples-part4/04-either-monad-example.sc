import functional.part3.applicative._
import functional.part3.monad._
import functional.part4.eitherMonad._

// doing it here because the current dotty version has some issues with the given imports
// TODO: upgrade the compiler to a newer version
given[E] as Monad[[X] =>> Either[E, X]] {
  override def pure[A](a: A): Either[E, A] = Right(a)
  override def flatMap[A, B](ma: Either[E, A], f: A => Either[E, B]): Either[E, B] = ma.flatMap(f)
}


// Arithmetic with reasons why it failed

case class ArithmeticError(msg: String)

type Arithmetic[A] =  Either[ArithmeticError, A]
def fail(msg: String) = Left(ArithmeticError(msg))
def success[A](a: A) = Right(a)
def pure[A](a: A) = Monad[Arithmetic].pure(a)

def safeDiv(a: Double, b: Double): Arithmetic[Double] = {
  if (b == 0) {
    fail("can't divide by zero")
  } else {
    success(a / b)
  }
}

safeDiv(5, 7)
safeDiv(5, 0)

def safeSqrt(v: Double): Arithmetic[Double] = {
  if (v < 0) {
    fail("can't square negative values, this will produce an imaginary number")
  } else {
    success(math.sqrt(v))
  }
}

safeSqrt(9)
safeSqrt(4)
safeSqrt(-4)


def squreDiv(a: Double, b: Double): Arithmetic[Double] = for {
  sa <- safeSqrt(a);
  sb <- safeSqrt(b);
  r <- safeDiv(sa, sb)
} yield r


squreDiv(9, 3)
squreDiv(-9, 3)
squreDiv(9, 0)

